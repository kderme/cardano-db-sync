module Cardano.Mock.Generator where

import           Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus

import qualified Cardano.Mock.Chain as Chain

genGenesis :: Gen (ExtLedgerState c)
genGenesis = undefined

data Config = 
extendChain :: Chain blk -> Gen (Chain c, Block c)
Chain blk -> Gen 
