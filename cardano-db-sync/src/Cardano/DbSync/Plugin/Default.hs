{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertEpochRewards, insertEpochStake,
                   insertShelleyBlock)
import           Cardano.DbSync.Rollback (rollbackToSlot)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- | The default SyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
defDbSyncNodePlugin backend =
  SyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = insertDefaultBlock backend
    , plugInsertBlockDetails = []
    , plugRollbackBlock = [rollbackToSlot backend]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: SqlBackend -> Trace IO Text -> SyncEnv -> [CardanoBlock]
    -> ExceptT SyncNodeError IO [BlockDetails]
insertDefaultBlock backend tracer env blocks =
    mapExceptT (DB.runDbIohkLogging backend tracer) $
      mapM insert blocks
  where
    insert
        :: (MonadBaseControl IO m, MonadIO m)
        => CardanoBlock -> ExceptT SyncNodeError (ReaderT SqlBackend m) BlockDetails
    insert cblk = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let network = leNetwork (envLedger env)
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk
      let details = lssSlotDetails lStateSnap
      handleLedgerEvents tracer (envLedger env) (lssEvents lStateSnap)
      case cblk of
        BlockByron blk ->
          newExceptT $ insertByronBlock tracer blk details
        BlockShelley blk ->
          newExceptT $ insertShelleyBlock tracer network (Generic.fromShelleyBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertShelleyBlock tracer network (Generic.fromAllegraBlock blk) lStateSnap details
        BlockMary blk ->
          newExceptT $ insertShelleyBlock tracer network (Generic.fromMaryBlock blk) lStateSnap details
      -- Now we update it in ledgerStateVar and (possibly) store it to disk.
      liftIO $ saveLedgerStateMaybe (envLedger env)
                    lStateSnap (isSyncedWithinSeconds details 60)
      pure $ BlockDetails cblk details

handleLedgerEvents
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
handleLedgerEvents tracer lenv =
    mapM_ printer
  where
    printer
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    printer ev =
      case ev of
        LedgerNewEpoch en ->
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerRewards details rwds -> do
          let progress = calcEpochProgress 4 details
          when (progress > 0.6) $
            liftIO . logInfo tracer $ mconcat [ "LedgerRewards: ", textShow progress ]
          insertEpochRewards tracer lenv rwds
        LedgerStakeDist sdist ->
          insertEpochStake tracer lenv sdist

calcEpochProgress :: Int -> SlotDetails -> Double
calcEpochProgress digits sd =
  let factor = 10 ^ digits
      dval = fromIntegral (unEpochSlot $ sdEpochSlot sd) / fromIntegral (unEpochSize $ sdEpochSize sd)
  in fromIntegral (floor (dval * factor) :: Int) / factor
