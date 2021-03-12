{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Mock.Chain where

import           Control.Exception (assert)
import           Control.Monad.Except
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import           Ouroboros.Consensus.Ledger.SupportsProtocol

import           Ouroboros.Network.Block (ChainUpdate (..), Tip (..), genesisPoint)

data ChainDB block = ChainDB
  { chainConfig :: TopLevelConfig block
  , cchain :: Maybe (Chain block)
  }

instance (Eq (Chain blk)) => Eq (ChainDB blk) where
  a == b = cchain a == cchain b

instance (Show (Chain blk)) => Show (ChainDB blk) where
  show = show . cchain

type State block = Consensus.ExtLedgerState block

initChainDB :: TopLevelConfig block -> ChainDB block
initChainDB config = ChainDB config Nothing

data Chain' block st =
    Genesis st
  | Chain' block st :> (block, st)
  deriving (Eq, Ord, Show, Functor)

type Chain block = Chain' block (State block)

infixl 5 :>

headTip :: HasHeader block => ChainDB block -> Tip block
headTip chainDB = case cchain chainDB of
  Nothing -> error "headTip for uninitiated ChainDB"
  Just (Genesis _) -> TipGenesis
  Just (_ :> (b, _)) -> Tip (blockSlot b) (blockHash b) (blockNo b)

insertGenesis :: ChainDB block -> State block -> ChainDB block
insertGenesis chainDB st = case cchain chainDB of
  Nothing -> chainDB {cchain = Just $ Genesis st}
  Just _ -> error "Tried to insertGenesis to an initiated chain"

extendChain :: LedgerSupportsProtocol block => ChainDB block -> block -> ChainDB block
extendChain chainDB blk = case cchain chainDB of
  Nothing -> error "can't apply block to an Uninitiated chain"
  Just chain ->
    case runExcept $ tickThenApply (Consensus.ExtLedgerCfg $ chainConfig chainDB) blk (getTipState chain) of
      Left err -> error $ show err
      Right st -> chainDB {cchain = Just $ chain :> (blk, st)}

getTipState :: Chain' blk st -> st
getTipState (Genesis st) = st
getTipState (_ :> (_, st)) = st

data ChainProducerState block = ChainProducerState
  { chainDB        :: ChainDB block
  , chainFollowers :: FollowerStates block
  , nextFollowerId :: FollowerId
  }

type FollowerId = Int

type FollowerStates block = Map FollowerId (FollowerState block)

data FollowerState block = FollowerState
  { -- | Where the chain of the consumer and producer intersect. If the
    -- consumer is on the chain then this is the consumer's chain head,
    -- but if the consumer's chain is off the producer's chain then this is
    -- the point the consumer will need to rollback to.
    followerPoint :: Point block
  , -- | Where the will go next, roll back to the follower point, or roll
    -- forward from the follower point.
    followerNext  :: FollowerNext
  }
  deriving (Eq, Show)

data FollowerNext
  = FollowerBackTo
  | FollowerForwardFrom
  deriving (Eq, Show)

initChainProducerState :: TopLevelConfig block -> ChainProducerState block
initChainProducerState config = ChainProducerState (initChainDB config) Map.empty 0

successorMBlock :: forall block . HasHeader block => Point block -> Maybe (Chain block) -> Maybe block
successorMBlock _ Nothing = Nothing
successorMBlock point (Just c) = successorBlock point c

successorBlock :: forall block . HasHeader block => Point block -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 =
    go c0
  where
    go :: Chain block -> Maybe block
    go (c :> (b',st') :> (b, _)) | blockPoint b' == p = Just b
                          | otherwise          = go (c :> (b',st'))
    go (Genesis _ :> (b, _))   | p == genesisPoint  = Just b
    go _ = error "successorBlock: point not on chain"

-- | What a follower needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. It also updates
-- the producer's state assuming that the follower follows its instruction.
--
followerInstruction :: HasHeader block
                  => FollowerId
                  -> ChainProducerState block
                  -> Maybe (ChainUpdate block block, ChainProducerState block)
followerInstruction fid cps@(ChainProducerState c cflrst cfid) =
    let FollowerState {followerPoint, followerNext} = lookupFollower cps fid in
    case followerNext of
      FollowerForwardFrom ->
          assert (pointOnMChain followerPoint $ cchain c) $
          case successorMBlock followerPoint $ cchain c of
            -- There is no successor block because the follower is at the head
            Nothing -> Nothing

            Just b -> Just (AddBlock b, cps')
              where
                cps' = ChainProducerState c (Map.adjust setPoint fid cflrst) cfid
                setPoint flrst = flrst { followerPoint = blockPoint b }

      FollowerBackTo -> Just (RollBack followerPoint, cps')
        where
          cps' = ChainProducerState c (Map.adjust setForwardFrom fid cflrst) cfid
          setForwardFrom flrst = flrst { followerNext = FollowerForwardFrom }

-- | Get the recorded state of a chain consumer. The 'FollowerId' is assumed to
-- exist.
--
lookupFollower :: ChainProducerState block -> FollowerId -> FollowerState block
lookupFollower (ChainProducerState _ cflrst _) fid = cflrst Map.! fid

pointOnMChain :: HasHeader block => Point block -> Maybe (Chain block) -> Bool
pointOnMChain _ Nothing = False
pointOnMChain point (Just c) = pointOnChain point c

pointOnChain :: HasHeader block => Point block -> Chain block -> Bool
pointOnChain GenesisPoint               _           = True
pointOnChain (BlockPoint _ _)           (Genesis _) = False
pointOnChain p@(BlockPoint pslot phash) (c :> (b, _))
  | pslot >  blockSlot b = False
  | phash == blockHash b = True
  | otherwise            = pointOnChain p c

headPoint :: HasHeader block => Chain block -> Point block
headPoint (Genesis _) = genesisPoint
headPoint (_ :> (b, _))    = blockPoint b



-- | Add a new follower with the given intersection point and return the new
-- 'FollowerId'.
--
initFollower :: HasHeader block
             => Point block
             -> ChainProducerState block
             -> (ChainProducerState block, FollowerId)
initFollower point (ChainProducerState c cflrst cfid) =
    assert (pointOnMChain point $ cchain c)
      (ChainProducerState c (Map.insert cfid flrst cflrst) (succ cfid), cfid)
  where
    flrst = FollowerState
            { followerPoint = point
            , followerNext  = FollowerBackTo
            }
