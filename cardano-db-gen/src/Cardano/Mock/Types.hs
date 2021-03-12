module Cardano.Mock.Types where

import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus

import qualified Cardano.Mock.Chain as Chain

type Block c = HardForkBlock (Cardano.CardanoEras c)
type ExtLedgerState c = Consensus.ExtLedgerState (Block c)
type Chain c = Chain.Chain (Block c)
type Config c = TopLevelConfig (Block c)
