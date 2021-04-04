{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync.StateQuery
  ( querySlotDetails
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.Sync.Types

import           Cardano.Prelude hiding (atomically)

import           Data.Time.Clock (UTCTime, addUTCTime)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.HardFork.History.Qry (Expr (..), Qry, qryFromExpr,
                   slotToEpoch')

-- -------------------------------------------------------------------------------------------------

-- TODO: Switch back to the old version of this when this is fixed:
-- https://github.com/input-output-hk/cardano-db-sync/issues/276
querySlotDetails :: SystemStart -> SlotNo -> Qry SlotDetails
querySlotDetails start absSlot = do
  absTime <- qryFromExpr $
                ELet (EAbsToRelSlot (ELit absSlot)) $ \ relSlot ->
                ELet (ERelSlotToTime (EVar relSlot)) $ \ relTime ->
                ELet (ERelToAbsTime (EVar relTime)) $ \ absTime ->
                EVar absTime
  (absEpoch, slotInEpoch) <- slotToEpoch' absSlot
  epochSize <- qryFromExpr $ EEpochSize (ELit absEpoch)
  let time = relToUTCTime start absTime
  -- Set sdCurrentTime below and over write that above.
  pure $ SlotDetails time time absEpoch (EpochSlot slotInEpoch) epochSize

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start
