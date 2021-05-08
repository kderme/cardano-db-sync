{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Sync.Era.Shelley.Generic.ProtoParams
  ( ProtoParams (..)
  , epochProtoParams
  ) where

import           Cardano.Prelude

import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Alonzo as Alonzo

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Types

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra,
                   StandardAlonzo, StandardCrypto, StandardMary, StandardShelley)

import           Ouroboros.Consensus.Cardano (Nonce (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import           Shelley.Spec.Ledger.BaseTypes (UnitInterval)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import           Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.PParams as Shelley

data ProtoParams = ProtoParams
  { ppMinfeeA :: !Natural
  , ppMinfeeB :: !Natural
  , ppMaxBBSize :: !Natural
  , ppMaxTxSize :: !Natural
  , ppMaxBHSize :: !Natural
  , ppKeyDeposit :: !Coin
  , ppPoolDeposit :: !Coin
  , ppMaxEpoch :: !EpochNo
  , ppOptialPoolCount :: !Natural
  , ppInfluence :: !Rational
  , ppMonetaryExpandRate :: !UnitInterval
  , ppTreasuryGrowthRate :: !UnitInterval
  , ppDecentralisation :: !UnitInterval
  , ppExtraEntropy :: !Nonce
  , ppProtocolVersion :: !ProtVer
  , ppMinUTxOValue :: !Coin
  , ppMinPoolCost :: !Coin
  }

epochProtoParams :: ExtLedgerState CardanoBlock -> Maybe ProtoParams
epochProtoParams lstate =
    case ledgerState lstate of
      LedgerStateByron _ -> Nothing
      LedgerStateShelley sls -> Just $ shelleyProtoParams sls
      LedgerStateAllegra als -> Just $ allegraProtoParams als
      LedgerStateMary mls -> Just $ maryProtoParams mls
      LedgerStateAlonzo als -> Just $ alonzoProtoParams als

allegraProtoParams :: LedgerState (ShelleyBlock StandardAllegra) -> ProtoParams
allegraProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

alonzoProtoParams :: LedgerState (ShelleyBlock StandardAlonzo) -> ProtoParams
alonzoProtoParams =
  fromAlonzoParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

maryProtoParams :: LedgerState (ShelleyBlock StandardMary) -> ProtoParams
maryProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

shelleyProtoParams :: LedgerState (ShelleyBlock StandardShelley) -> ProtoParams
shelleyProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

-- -------------------------------------------------------------------------------------------------

fromAlonzoParams :: Alonzo.PParams (Alonzo.AlonzoEra StandardCrypto) -> ProtoParams
fromAlonzoParams = panic "Cardano.Sync.Era.Shelley.Generic.ProtoParams.fromAlonzoParams"

fromShelleyParams :: Shelley.PParams' Identity era -> ProtoParams
fromShelleyParams params =
  ProtoParams
    { ppMinfeeA = Shelley._minfeeA params
    , ppMinfeeB = Shelley._minfeeB params
    , ppMaxBBSize = Shelley._maxBBSize params
    , ppMaxTxSize = Shelley._maxTxSize params
    , ppMaxBHSize = Shelley._maxBHSize params
    , ppKeyDeposit = Shelley._keyDeposit params
    , ppPoolDeposit = Shelley._poolDeposit params
    , ppMaxEpoch = Shelley._eMax params
    , ppOptialPoolCount = Shelley._nOpt params
    , ppInfluence = Shelley._a0 params
    , ppMonetaryExpandRate = Shelley._rho params
    , ppTreasuryGrowthRate = Shelley._tau params
    , ppDecentralisation  = Shelley._d params
    , ppExtraEntropy = Shelley._extraEntropy params
    , ppProtocolVersion = Shelley._protocolVersion params
    , ppMinUTxOValue = Shelley._minUTxOValue params
    , ppMinPoolCost = Shelley._minPoolCost params
    }
