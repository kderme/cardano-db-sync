{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Era.Shelley.Offline
  ( runOfflineFetchThread
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Control.Monad.Trans.Except.Extra (handleExceptT, left)

import           Cardano.DbSync.Era.Shelley.Offline.FetchQueue
import           Cardano.DbSync.Era.Shelley.Offline.Http
import           Cardano.DbSync.Era.Shelley.Offline.Query
import           Cardano.DbSync.Era.Shelley.Offline.Types

import           Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Db
import qualified Cardano.Db as DB

import           Cardano.Sync.Util

import           Database.Persist.Sql (SqlBackend)

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as Http


data FetchLoopType
    = FetchLoopForever
    | FetchLoopOnce
    deriving (Eq, Show)

-- This is run as a new thread with the fetchLoop looping forever.
runOfflineFetchThread :: Trace IO Text -> SqlBackend -> IO ()
runOfflineFetchThread trce backend = do
  threadDelay 60_000_000 -- 60 seconds
  logInfo trce "Runing Offline fetch thread"
  fetchLoop trce backend FetchLoopForever

-- -------------------------------------------------------------------------------------------------

fetchLoop :: Trace IO Text -> SqlBackend -> FetchLoopType -> IO ()
fetchLoop trce sqlBackend fetchLoopType =
    case fetchLoopType of
      FetchLoopForever -> forever work
      FetchLoopOnce -> work
  where
    work :: IO ()
    work = do
      -- A interval pause so we don't do too much work on this thread.
      liftIO $ threadDelay 60_000_000 -- 60 seconds

      now <- liftIO Time.getPOSIXTime

      -- Fetch all pools that have not been run with success.
      -- This has to be stateful in order to count.
      pools <- liftIO $ runDbIohkLogging sqlBackend trce queryPoolFetchRetry

      let runnablePools = filter (isRunnable now) pools

      liftIO $ logInfo trce $
        mconcat
          [ " ***************************** ", "Pools with errors: total "
          , textShow (length pools), ", runnable ", textShow (length runnablePools), "."
          ]

      -- We actually run the fetch again.
      liftIO $ forM_ runnablePools $ \ pool ->
          fetchInsertNewPoolMetadataOld trce sqlBackend pool

    -- Filter for figuring out if it's time for a retry.
    isRunnable :: POSIXTime -> PoolFetchRetry -> Bool
    isRunnable now pfr = retryRetryTime (pfrRetry pfr) < now


-- Please note that it is possible that a user submits the same pool hash
-- several times which will result in the fetch being done for each of the entry.
-- We do this in order to make sure that the metadata wasn't fixed in the meantime.
fetchInsertNewPoolMetadataOld :: Trace IO Text -> SqlBackend -> PoolFetchRetry -> IO ()
fetchInsertNewPoolMetadataOld tracer sqlBackend pfr = do
    logInfo tracer $ showRetryTimes (pfrRetry pfr)

    res <- runExceptT $ fetchInsert tracer sqlBackend pfr

    -- In the case all went well, we do nothing, but if something went wrong
    -- we log that and add the error to the database.
    case res of
        Right () ->
            logInfo tracer "Pool metadata fetch successful!"

        Left err -> do
            let fetchError = renderFetchError err
            logWarning tracer $ "Pool metadata fetch failed: " <> fetchError

            DB.runDbIohkLogging sqlBackend tracer $
              void $ DB.insertPoolOfflineFetchError
                DB.PoolOfflineFetchError
                  { DB.poolOfflineFetchErrorPoolId = pfrPoolHashId pfr
                  , DB.poolOfflineFetchErrorFetchTime = Time.posixSecondsToUTCTime (retryFetchTime $ pfrRetry pfr)
                  , DB.poolOfflineFetchErrorPmrId = pfrReferenceId pfr
                  , DB.poolOfflineFetchErrorFetchError = fetchError
                  , DB.poolOfflineFetchErrorRetryCount = retryCount (pfrRetry pfr) + 1
                  }


-- |We pass in the @PoolIdent@ so we can know from which pool the error occured.
fetchInsert :: Trace IO Text -> SqlBackend -> PoolFetchRetry -> ExceptT FetchError IO ()
fetchInsert tracer sqlBackend pfr = do
    -- This is a bit bad to do each time, but good enough for now.
    manager <- liftIO $ Http.newManager tlsManagerSettings

    let poolMetadataURL = pfrPoolUrl pfr

    liftIO . logInfo tracer $ "Request URL: " <> unPoolUrl poolMetadataURL

    -- This is a weird Error.
    request <- handleExceptT (\(err :: HttpException) -> FEUrlParseFail (pfrPoolIdent pfr) poolMetadataURL (textShow err))
                $ Http.parseRequest (Text.unpack $ unPoolUrl poolMetadataURL)

    (respBS, status) <- httpGet512BytesMax (pfrPoolIdent pfr) poolMetadataURL request manager

    when (Http.statusCode status /= 200) .
      left $ FEHttpResponse (pfrPoolIdent pfr) poolMetadataURL (Http.statusCode status)

    liftIO . logInfo tracer $ "Response: " <> show (Http.statusCode status)

    decodedMetadata <-
        case eitherDecode' (LBS.fromStrict respBS) of
            Left err -> left $ FEJsonDecodeFail (pfrPoolIdent pfr) poolMetadataURL (Text.pack err)
            Right res -> pure res

    -- Let's check the hash
    let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
        expectedHash = unPoolMetaHash (pfrPoolMDHash pfr)

    if PoolMetaHash metadataHash /= pfrPoolMDHash pfr
      then left $ FEHashMismatch (pfrPoolIdent pfr) poolMetadataURL (renderByteArray expectedHash) (renderByteArray metadataHash)
      else liftIO . logInfo tracer $ "Inserting pool data with hash: " <> renderByteArray expectedHash

    void . liftIO . DB.runDbIohkLogging sqlBackend tracer $
        DB.insertPoolOfflineData
          DB.PoolOfflineData
            { DB.poolOfflineDataPoolId = pfrPoolHashId pfr
            , DB.poolOfflineDataTickerName = unPoolTicker $ pomTicker decodedMetadata
            , DB.poolOfflineDataHash = metadataHash
            , DB.poolOfflineDataMetadata = Text.decodeUtf8 respBS
            , DB.poolOfflineDataPmrId = pfrReferenceId pfr
            }

    liftIO $ logInfo tracer (decodeUtf8 respBS)

