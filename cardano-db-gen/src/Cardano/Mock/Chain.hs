{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Mock.Chain where

import           Control.Exception (assert)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Ouroboros.Consensus.Block

import           Ouroboros.Network.Block (ChainUpdate (..), Tip (..), genesisPoint)


data Chain block
  = Genesis
  | Chain block :> block
  deriving (Eq, Ord, Show, Functor)

infixl 5 :>

headTip :: HasHeader block => Chain block -> Tip block
headTip Genesis  = TipGenesis
headTip (_ :> b) = Tip (blockSlot b) (blockHash b) (blockNo b)

data ChainProducerState block = ChainProducerState
  { chainState     :: Chain block
  , chainFollowers :: FollowerStates block
  , nextFollowerId :: FollowerId
  }
  deriving (Eq, Show)

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

initChainProducerState :: ChainProducerState block
initChainProducerState = ChainProducerState Genesis Map.empty 0

successorBlock :: forall block . HasHeader block => Point block -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 =
    go c0
  where
    go :: Chain block -> Maybe block
    go (c :> b' :> b) | blockPoint b' == p = Just b
                      | otherwise          = go (c :> b')
    go (Genesis :> b) | p == genesisPoint  = Just b
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
          assert (pointOnChain followerPoint c) $
          case successorBlock followerPoint c of
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


pointOnChain :: HasHeader block => Point block -> Chain block -> Bool
pointOnChain GenesisPoint               _       = True
pointOnChain (BlockPoint _ _)           Genesis = False
pointOnChain p@(BlockPoint pslot phash) (c :> b)
  | pslot >  blockSlot b = False
  | phash == blockHash b = True
  | otherwise            = pointOnChain p c

headPoint :: HasHeader block => Chain block -> Point block
headPoint Genesis  = genesisPoint
headPoint (_ :> b) = blockPoint b



-- | Add a new follower with the given intersection point and return the new
-- 'FollowerId'.
--
initFollower :: HasHeader block
             => Point block
             -> ChainProducerState block
             -> (ChainProducerState block, FollowerId)
initFollower point (ChainProducerState c cflrst cfid) =
    assert (pointOnChain point c)
      (ChainProducerState c (Map.insert cfid flrst cflrst) (succ cfid), cfid)
  where
    flrst = FollowerState
            { followerPoint = point
            , followerNext  = FollowerBackTo
            }
