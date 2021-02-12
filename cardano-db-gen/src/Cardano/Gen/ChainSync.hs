{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Gen.ChainSync
  ( chainSyncServer
  , ChainProducerState (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, StrictTVar, atomically, readTVar,
                   retry, writeTVar)

-- import           Cardano.DbSync.Config.Types (CardanoBlock)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Network.Block (ChainUpdate (..), HasHeader (..), Point (..), Tip (..),
                   blockHash, blockNo, blockPoint, blockSlot, castPoint, castTip, genesisPoint,
                   pattern BlockPoint, pattern GenesisPoint, pattern TipGenesis)
import           Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer (..),
                   ServerStIdle (..), ServerStNext (..))

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


chainSyncServer
    :: forall blk m a.
        ( HasHeader blk
        , MonadSTM m
        )
    => a -> StrictTVar m (ChainProducerState blk)
    -> ChainSyncServer blk (Point blk) (Tip blk) m a
chainSyncServer recvMsgDoneClient chainvar =
    ChainSyncServer $ idle <$> newFollower
  where
    idle :: FollowerId -> ServerStIdle blk (Point blk) (Tip blk) m a
    idle r =
      ServerStIdle
        { recvMsgRequestNext = handleRequestNext r
        , recvMsgFindIntersect =  error "chainSyncServer: Intersections not supported!"
        , recvMsgDoneClient = pure recvMsgDoneClient
        }

    idle' :: FollowerId -> ChainSyncServer blk (Point blk) (Tip blk) m a
    idle' = ChainSyncServer . pure . idle

    handleRequestNext :: FollowerId
                      -> m (Either (ServerStNext blk (Point blk) (Tip blk) m a)
                                (m (ServerStNext blk (Point blk) (Tip blk) m a)))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> pure (Left  (sendNext r update))
        Nothing     -> pure (Right (sendNext r <$> readChainUpdate r))
                       -- Follower is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: FollowerId
             -> (Tip blk, ChainUpdate blk blk)
             -> ServerStNext blk (Point blk) (Tip blk) m a
    sendNext r (tip, AddBlock b) = SendMsgRollForward  b             tip (idle' r)
    sendNext r (tip, RollBack p) = SendMsgRollBackward (castPoint p) tip (idle' r)

    newFollower :: m FollowerId
    newFollower = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = initFollower genesisPoint cps
      writeTVar chainvar cps'
      pure rid

    tryReadChainUpdate :: FollowerId
                       -> m (Maybe (Tip blk, ChainUpdate blk blk))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case followerInstruction rid cps of
          Nothing -> pure Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = chainState cps'
            pure $ Just (castTip (headTip chain), u)

    readChainUpdate :: FollowerId -> m (Tip blk, ChainUpdate blk blk)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case followerInstruction rid cps of
          Nothing -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = chainState cps'
            pure (castTip (headTip chain), u)


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





{-

Erik de Castro Lopo  1 day ago
@channel to test dn-sync, i need to generate blocks (initially only Byron, but
later Shelley/Allegra/Mary as well) and set up the node end of the ChainSync
protocol so I can feed the generated blocks to the db-sync target.
I would appreciate it if someone could point me in the direction of the code I
need to look at to implement this.
it would be even better if there was an example or test they could point me at

Marcin Szamotulski  1 day ago
The benchmarking team have some generators, or ask the consensus team how they
do that.

Erik de Castro Lopo  1 day ago
i do not need generators. i need to know how to push generated block through the
ChainSync protocol (edited)

Marcin Szamotulski  1 day ago
Then look at Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample
in ouroboros-network package, the server that runs in the node is another
example: Ouroboros.Consensus.MiniProtocol.ChainSync.Server in ouroboros-consensus
package (but it probably has to many details you're not interested in).

Erik de Castro Lopo  19 hours ago
@marcin chainSyncServerExample has the following type signature:

chainSyncServerExample :: forall blk header m a.
                          ( HasHeader header
                          , MonadSTM m
                          , HeaderHash header ~ HeaderHash blk
                          )
                       => a
                       -> StrictTVar m (ChainProducerState header)
                       -> ChainSyncServer header (Point blk) (Tip blk) m a

why does part of that use header and other parts use blk ?
am I supposed to reuse that function or use it as an example to write something to suite my needs?

Marcin Szamotulski  15 hours ago
This is to support the scenario that is used by the node-to-node protocol:
chain-sync only serves headers, but Tip and Point correspond to blk type.

Marcin Szamotulski  15 hours ago
In node-to-client you can assume header ~ blk.
@erikd That function as written assumes that the entire chain to be served is
available as a linked list (cf initChainProducerState and switchFork in module
Ouroboros.Network.MockChain.ProducerState and Ouroboros.Network.MockChain.Chain.Chain
versus the argument to chainSyncServerExample).My hunch is you don't want to shoe
horn your block/chain source into a pure linked list, so you'll probably want to use
the 90 lines of the chainSyncServerExample definition as a template, replacing
the calls to ChainProducerState.followerInstruction et al as needed to access
your chain source.I also wonder whether your use case might be simpler than even
this minimal yet fully fledged ChainSync server: do you need the intersection
finding and the rollbacks etc? HTH. (edited)

Erik de Castro Lopo  1 hour ago
thanks @nfrisby , that is the most useful input I have received on this issue so far
i do not need the intersection part, but i would like to test rollbacks

Nicolas Frisby  1 hour ago
Nice: you'll be able to set recvMsgFindIntersect = error "we don't need no intersections!".
Otherwise Beyond that, the shape of recvMsgRequestNext should basically stay the same,
mutatis mutandi for replacing the TVar ChainProducerState with your source of truth for
the "server's current chain". (edited)

Nicolas Frisby  1 hour ago
There are more examples in the network layer somewhere of actually converting this
a state machine definition such as chainSyncServerExample into a thread that will
respond to messages (eg on a socket, or via some MVar, etc etc). But I've never
internalized them. @marcin Is there another pointer along those lines you would suggest?

Erik de Castro Lopo  1 hour ago
thanks, will try that as soon as I get back (will be AFK for 2-3 hours)

Nicolas Frisby  1 hour ago
OK, good luck. I anticipate that I'll loop back around on this tomorrow at the earliest...
my TODO list remains at an intimidating size :smile: (edited)


-}
