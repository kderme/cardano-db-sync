{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Mock.ServerHandle where

import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, STM, StrictTVar, newTVarIO, readTVar)
import           Cardano.Mock.Chain

import qualified Ouroboros.Consensus.Ledger.Extended as Consensus

newtype ServerHandle m blk = ServerHandle
  { chainProducerState :: StrictTVar m (ChainProducerState blk)
  }

mkServerHandle :: MonadSTM m => Consensus.ExtLedgerState blk -> m (ServerHandle m blk)
mkServerHandle st = do
  chainSt <- newTVarIO (initChainProducerState st)
  return $ ServerHandle chainSt

readChain :: MonadSTM m => ServerHandle m blk -> STM m (Chain blk)
readChain handle = do
  chainState <$> readTVar (chainProducerState handle)
