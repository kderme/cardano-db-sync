module Cardano.Db.Tool.PrepareSnapshot
  ( PrepareSnapshotArgs (..)
  , prepareSnapshot
  ) where

import           Cardano.Prelude (Word64, fromMaybe)

import           Control.Monad

import           Cardano.Db
import           Cardano.Sync.Config.Types hiding (LogFileDir, MigrationDir)
import           Cardano.Sync.LedgerState

import qualified Data.ByteString.Base16 as Base16

import           Ouroboros.Network.Block hiding (blockHash)

data PrepareSnapshotArgs = PrepareSnapshotArgs
  { psLedgerStateDir :: LedgerStateDir
  }

prepareSnapshot :: PrepareSnapshotArgs -> IO ()
prepareSnapshot = prepareSnapshotAux True

prepareSnapshotAux :: Bool -> PrepareSnapshotArgs -> IO ()
prepareSnapshotAux firstTry args = do
    ledgerFiles <- listLedgerStateFilesOrdered (psLedgerStateDir args)
    mblock <- runDbNoLogging queryLatestBlock
    case mblock of
      Just block | Just bSlotNo <- SlotNo <$> blockSlotNo block -> do
        let bHash = blockHash block
        let (newerFiles, mfile) = findLedgerStateFile ledgerFiles (bSlotNo, bHash)
        printNewerSnapshots newerFiles
        case mfile of
            Right file -> do
              let bblockNo = fromMaybe 0 $ blockBlockNo block
              printCreateSnapshot bblockNo (lsfFilePath file)
            Left (file : _) -> do
                -- We couldn't find the tip of the db, so we return a list of
                -- the available ledger files, before this tip.
              putStrLn $ concat
                [ "Ledger and db don't match. DB tip is at "
                , show bSlotNo, " ", show (hashToAnnotation bHash)
                , " (full ", show (Base16.encode bHash), ")"
                , " and the closest ledger state file is at "
                , show (lsfSlotNo file), " ", show (lsfHash file)
                ]
              if firstTry then do
                interactiveRollback $ lsfSlotNo file
                prepareSnapshotAux False args
              else
                putStrLn "After a rollback the db is in sync with no ledger state file"
            Left [] ->
              putStrLn "No ledger state file matches the db tip. You need to run db-sync before creating a snapshot"

      _ -> do
            putStrLn "The db is empty. You need to sync from genesis and then create a snapshot."
  where
    interactiveRollback :: SlotNo -> IO ()
    interactiveRollback slot = do
      putStrLn $ "Do you want to rollback the db to " ++ show slot  ++ " (Y/n)"
      input <- getLine
      case input of
        "n" -> return ()
        _ -> do
          putStrLn $ "Rolling back to " ++ show slot
          runRollback slot
          putStrLn $ "Rolling back done. Revalidating from scratch"
          putStrLn ""

    runRollback :: SlotNo -> IO ()
    runRollback slot = runDbNoLogging $ do
      slots <- querySlotNosGreaterThan $ unSlotNo slot
      mapM_ deleteCascadeSlotNo slots

    printNewerSnapshots :: [LedgerStateFile] -> IO ()
    printNewerSnapshots newerFiles = do
      when (not $ null newerFiles) $ do
        putStrLn $ concat
          [ "There are newer ledger state files, which are ignored: "
          , show newerFiles
          ]
        putStrLn ""

    printCreateSnapshot :: Word64 -> FilePath -> IO ()
    printCreateSnapshot bblockNo fp = do
      putStrLn "Create a snapshot with"
      putStrLn $ concat
        [ "    scripts/postgresql-setup.sh --create-snapshot db-sync-data-<MVersion>-block-"
        , show bblockNo
        , "-x86_64 "
        , fp
        ]
