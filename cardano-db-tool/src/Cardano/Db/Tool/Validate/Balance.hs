{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Tool.Validate.Balance
  ( ledgerAddrBalance
  ) where

import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Chain.Block as Byron
import           Cardano.Chain.Common (CompactAddress, Lovelace, decodeAddressBase58, sumLovelace,
                   toCompactAddress, unsafeGetLovelace)
import qualified Cardano.Chain.UTxO as Byron

import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Era (Crypto)

import           Cardano.Ledger.Compactible
import           Cardano.Ledger.Val
import           Cardano.Prelude

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

import           Shelley.Spec.Ledger.CompactAddr (CompactAddr, compactAddr)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Shelley.Spec.Ledger.API (Addr (..), Coin (..))
import           Shelley.Spec.Ledger.Address (BootstrapAddress (..))

-- Given an address, return it's current UTxO balance.
ledgerAddrBalance :: Text -> LedgerState (CardanoBlock StandardCrypto) -> Either Text Word64
ledgerAddrBalance addr lsc =
    case lsc of
      LedgerStateByron st -> getByronBalance addr $ Byron.cvsUtxo $ byronLedgerState st
      LedgerStateShelley st -> getShelleyBalance addr $ getUTxO st
      LedgerStateAllegra st -> getShelleyBalance addr $ getUTxO st
      LedgerStateMary st -> getShelleyBalance addr $ getUTxO st
  where
    getUTxO :: LedgerState (ShelleyBlock era) -> Shelley.UTxO era
    getUTxO = Shelley._utxo . Shelley._utxoState . Shelley.esLState . Shelley.nesEs . shelleyLedgerState

getByronBalance :: Text -> Byron.UTxO -> Either Text Word64
getByronBalance addrText utxo = do
    case toCompactAddress <$> decodeAddressBase58 addrText of
      Left err -> Left $ textShow err
      Right caddr -> bimap show unsafeGetLovelace . sumLovelace . mapMaybe (compactTxOutValue caddr) . Map.elems $ Byron.unUTxO utxo
  where
    compactTxOutValue :: CompactAddress -> Byron.CompactTxOut -> Maybe Lovelace
    compactTxOutValue caddr (Byron.CompactTxOut bcaddr lovelace) =
      if caddr == bcaddr
        then Just lovelace
        else Nothing

getShelleyBalance
    :: forall era. Ledger.TxOut era ~ Shelley.TxOut era -- somewhere in ledger-spec, there is probably a better constraint synonym for these
    => Compactible (Ledger.Value era) => Val (Ledger.Value era)
    => Text -> Shelley.UTxO era -> Either Text Word64
getShelleyBalance addrText utxo = do
    caddr <- getCompactAddress addrText
    Right . fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue caddr) (Map.elems $ Shelley.unUTxO utxo)
  where
    compactTxOutValue :: CompactAddr (Crypto era) -> Ledger.TxOut era -> Maybe Coin
    compactTxOutValue caddr (Shelley.TxOutCompact scaddr v) =
      if caddr == scaddr
        then Just $ coin (fromCompact v)
        else Nothing

getCompactAddress :: Text -> Either Text (CompactAddr c)
getCompactAddress addrText = case Api.deserialiseAddress (Api.AsAddress Api.AsShelleyAddr) addrText of
    Nothing ->
      case decodeAddressBase58 addrText of
        Left err -> Left $ textShow err
        Right badrr -> Right $ compactAddr (AddrBootstrap $ BootstrapAddress badrr)
    Just (Api.ShelleyAddress n p s) ->
      let addr = Addr n (coerce p) (coerce s)
      in Right $ compactAddr addr

textShow :: Show a => a -> Text
textShow = Text.pack . show
