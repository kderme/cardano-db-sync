{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Offline.Http
  ( FetchError (..)
  , httpGet512BytesMax
  , renderFetchError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleExceptT, hoistEither)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import           Cardano.Db

import           Cardano.Sync.Util

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http


-- |Fetch error for the HTTP client fetching the pool.
data FetchError
  = FEHashMismatch !PoolIdent !PoolUrl !Text !Text
  | FEDataTooLong !PoolIdent !PoolUrl
  | FEUrlParseFail !PoolIdent !PoolUrl !Text
  | FEJsonDecodeFail !PoolIdent !PoolUrl !Text
  | FEHttpException !PoolIdent !PoolUrl !Text
  | FEHttpResponse !PoolIdent !PoolUrl !Int
  | FEIOException !Text
  | FETimeout !PoolIdent !PoolUrl !Text
  | FEConnectionFailure !PoolIdent !PoolUrl
  deriving (Eq, Generic)

renderFetchError :: FetchError -> Text
renderFetchError fe =
  case fe of
    FEHashMismatch (PoolIdent pool) (PoolUrl url) xpt act ->
      mconcat
        [ "Hash mismatch from ", pool, " when fetching metadata from ", url
        , ". Expected ", xpt, " but got ", act, "."
        ]
    FEDataTooLong (PoolIdent pool) (PoolUrl url) ->
      mconcat
        [ "Offline pool data from ", pool, " when fetching metadata from ", url
        , " exceeded 512 bytes."
        ]
    FEUrlParseFail (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "URL parse error from ", pool, " when fetching metadata from ", url
        , "' resulted in : ", err
        ]
    FEJsonDecodeFail (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "JSON decode error from ", pool, " when fetching metadata from ", url
        , " resulted in : ", err
        ]
    FEHttpException (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "HTTP Exception from ", pool, " when fetching metadata from ", url
        , " resulted in : ", err
        ]
    FEHttpResponse (PoolIdent pool) (PoolUrl url) sc ->
      mconcat
        [ "HTTP Response from ", pool, " when fetching metadata from ", url
        , " resulted in : ", textShow sc
        ]
    FETimeout (PoolIdent pool) (PoolUrl url) ctx ->
      mconcat
        [ ctx, " timeout from ", pool, " when fetching metadata from "
        , url, "."
        ]
    FEConnectionFailure (PoolIdent pool) (PoolUrl url) ->
      mconcat
        [ "Connection failure from pool ", pool, " when fetching metadata from "
        , url, "'."
        ]
    FEIOException err -> "IO Exception: " <> err

httpGet512BytesMax
    :: PoolIdent -> PoolUrl -> Http.Request -> Http.Manager
    -> ExceptT FetchError IO (ByteString, Http.Status)
httpGet512BytesMax pool url request manager = do
    res <- handleExceptT (convertHttpException pool url) httpGet
    hoistEither res
  where
    httpGet :: IO (Either FetchError (ByteString, Http.Status))
    httpGet =
      Http.withResponse request manager $ \responseBR -> do
        -- We read the first chunk that should contain all the bytes from the reponse.
        responseBSFirstChunk <- Http.brReadSome (Http.responseBody responseBR) 512
        -- If there are more bytes in the second chunk, we don't go any further since that
        -- violates the size constraint.
        responseBSSecondChunk <- Http.brReadSome (Http.responseBody responseBR) 1
        if LBS.null responseBSSecondChunk
          then pure $ Right (LBS.toStrict responseBSFirstChunk, Http.responseStatus responseBR)
          else pure $ Left (FEDataTooLong pool url)

convertHttpException :: PoolIdent -> PoolUrl -> HttpException -> FetchError
convertHttpException pool url he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout -> FETimeout pool url "Response"
        Http.ConnectionTimeout -> FETimeout pool url "Connection"
        Http.ConnectionFailure {} -> FEConnectionFailure pool url
        other -> FEHttpException pool url (show other)
    InvalidUrlException urlx err -> FEUrlParseFail pool (PoolUrl $ Text.pack urlx) (Text.pack err)
