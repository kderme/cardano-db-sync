{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Mock.Server
  ( -- * server
    forkServerThread

    -- * ServerHandle api
  , ServerHandle (..)
  , MockServerConstraint
  , addGenesis
  , addBlock
  , readChain
  , stopServer
  ) where

import           Codec.CBOR.Write (toLazyByteString)

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadSTM.Strict (modifyTVar, readTVar, writeTVar, MonadSTM(atomically), MonadSTMTx(retry) )
import           Control.Concurrent.Async
import           Control.Tracer (nullTracer)
import           Control.Monad.Class.MonadSTM.Strict (STM, StrictTVar, newTVarIO)
import           Cardano.Mock.Chain

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import           Network.TypedProtocol.Core (Peer (..))

import           Ouroboros.Consensus.Block (castPoint, Point, CodecConfig, HasHeader)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Network.NodeToClient (Apps (..), Codecs' (..), DefaultCodecs)
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import           Ouroboros.Consensus.Node (ConnectionId)
import           Ouroboros.Consensus.Node.DbLock ()
import           Ouroboros.Consensus.Node.DbMarker ()
import           Ouroboros.Consensus.Node.InitStorage ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion, NodeToClientVersion, SupportedNetworkProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo ()
import           Ouroboros.Consensus.Node.Recovery ()
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Node.Tracers ()
import           Ouroboros.Consensus.Util.Args ()

import           Ouroboros.Network.Block (genesisPoint, Serialised (..), castTip, ChainUpdate(RollBack, AddBlock),  Tip, Serialised)
import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MuxMode (..), OuroborosApplication)
import           Ouroboros.Network.NodeToClient (NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (Versions)
import           Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (debuggingNetworkServerTracers)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy, Proxy(..))

import           Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer, ServerStIntersect (..), ServerStNext(SendMsgRollForward, SendMsgRollBackward),  ServerStIdle (..), ChainSyncServer (..), chainSyncServerPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)


data ServerHandle m blk = ServerHandle
  { chainProducerState :: StrictTVar m (ChainProducerState blk)
  , threadHandle :: Async ()
  }

instance Eq (ServerHandle m blk) where
  sh1 == sh2 = threadHandle sh1 == threadHandle sh2

addGenesis :: MonadSTM m => ServerHandle m blk -> State blk -> STM m ()
addGenesis handle st =
  modifyTVar (chainProducerState handle) $ \cps ->
    cps {chainDB = insertGenesis (chainDB cps) st}

readChain :: MonadSTM m => ServerHandle m blk -> STM m (Maybe (Chain blk))
readChain handle = do
  cchain . chainDB <$> readTVar (chainProducerState handle)

addBlock :: (LedgerSupportsProtocol blk, MonadSTM m) => ServerHandle m blk -> blk -> STM m ()
addBlock handle blk =
  modifyTVar (chainProducerState handle) $ \cps ->
    cps {chainDB = extendChain (chainDB cps) blk}

stopServer :: ServerHandle m blk -> IO ()
stopServer = cancel . threadHandle

type MockServerConstraint blk =
    ( HasHeader blk
    , ShowQuery (Query blk)
    , ShowProxy blk
    , ShowProxy (ApplyTxErr blk)
    , ShowProxy (GenTx blk)
    , ShowProxy (Query blk)
    , SerialiseNodeToClientConstraints blk
    , SupportedNetworkProtocolVersion blk
    )

forkServerThread
    :: forall blk.
       ( HasHeader blk
       , ShowQuery (Query blk)
       , ShowProxy blk
       , ShowProxy (ApplyTxErr blk)
       , ShowProxy (GenTx blk)
       , ShowProxy (Query blk)
       , SerialiseNodeToClientConstraints blk
       , SupportedNetworkProtocolVersion blk
       )
    => TopLevelConfig blk
    -> NetworkMagic
    -> FilePath
    -> IO (ServerHandle IO blk)
forkServerThread config networkMagic path = do
    putStrLn $ "+++++++++++++++++++++++++++++++++++++++++ starting Server: " ++ show path
    chainSt <- newTVarIO $ initChainProducerState config
    thread <- async $ runLocalServer networkMagic path chainSt
    return $ ServerHandle chainSt thread


-- | Must be called from the main thread
runLocalServer
    :: forall blk.
       ( HasHeader blk
       , ShowQuery (Query blk)
       , ShowProxy blk
       , ShowProxy (ApplyTxErr blk)
       , ShowProxy (GenTx blk)
       , ShowProxy (Query blk)
       , SerialiseNodeToClientConstraints blk
       , SupportedNetworkProtocolVersion blk
       )
    => NetworkMagic
    -> FilePath
    -> StrictTVar IO (ChainProducerState blk)
    -> IO ()
runLocalServer networkMagic localDomainSock chainProducerState =
    withIOManager $ \ iom ->
      withSnocket iom localDomainSock $ \ localSocket localSnocket -> do
                networkState <- NodeToClient.newNetworkMutableState
                _ <- NodeToClient.withServer
                       localSnocket
                       debuggingNetworkServerTracers -- TODO: some tracing might be useful.
                       networkState
                       localSocket
                       (versions chainProducerState)
                       NodeToClient.networkErrorPolicies
                return ()

  where
    versions :: StrictTVar IO (ChainProducerState blk)
             -> Versions NodeToClientVersion
                         NodeToClientVersionData
                         (OuroborosApplication 'ResponderMode LocalAddress ByteString IO Void ())
    versions state = combineVersions
            [ simpleSingletonVersions
                  version
                  (NodeToClientVersionData networkMagic)
                  (NTC.responder version
                    $ mkApps state version blockVersion (NTC.defaultCodecs codecConfig blockVersion version))
            | (version, blockVersion) <- Map.toList $ supportedNodeToClientVersions (Proxy @blk)
            ]

    codecConfig :: CodecConfig blk
    codecConfig = undefined

    mkApps :: StrictTVar IO (ChainProducerState blk) -> NodeToClientVersion -> BlockNodeToClientVersion blk -> DefaultCodecs blk IO
           -> NTC.Apps IO (ConnectionId addrNTC) ByteString ByteString ByteString ()
    mkApps chain _version blockVersion Codecs {..}  = Apps {..}
      where
        aChainSyncServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aChainSyncServer _them channel =
          runPeer
            nullTracer -- TODO add a tracer!
            cChainSyncCodec
            channel
            $ chainSyncServerPeer
            $ chainSyncServer chain codecConfig blockVersion

        aTxSubmissionServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aTxSubmissionServer _them channel =
          runPeer
            nullTracer
            cTxSubmissionCodec
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

        aStateQueryServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aStateQueryServer _them channel =
          runPeer
            nullTracer
            cStateQueryCodec
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

chainSyncServer
    :: forall blk m.
        ( HasHeader blk
        , MonadSTM m
        , SerialiseNodeToClient blk blk
        )
    => StrictTVar m (ChainProducerState blk)
    -> CodecConfig blk
    -> BlockNodeToClientVersion blk
    -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncServer state codec blockVersion =
    ChainSyncServer $ idle <$> newFollower
  where
    idle :: FollowerId -> ServerStIdle (Serialised blk) (Point blk) (Tip blk) m ()
    idle r =
      ServerStIdle
        { recvMsgRequestNext = handleRequestNext r
        , recvMsgFindIntersect = handleFindIntersect r
        , recvMsgDoneClient = pure ()
        }

    idle' :: FollowerId -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
    idle' = ChainSyncServer . pure . idle

    handleRequestNext :: FollowerId
                      -> m (Either (ServerStNext (Serialised blk) (Point blk) (Tip blk) m ())
                                (m (ServerStNext (Serialised blk) (Point blk) (Tip blk) m ())))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> pure (Left  (sendNext r update))
        Nothing     -> pure (Right (sendNext r <$> readChainUpdate r))
                       -- Follower is at the head, have to block and wait for
                       -- the producer's state to change.

    handleFindIntersect :: FollowerId -> [Point blk] -> m (ServerStIntersect (Serialised blk) (Point blk) (Tip blk) m ())
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip) -> return $ SendMsgIntersectFound     pt tip (idle' r)
        (Nothing, tip) -> return $ SendMsgIntersectNotFound     tip (idle' r)

    improveReadPoint :: FollowerId
                     -> [Point blk]
                     -> m (Maybe (Point blk), Tip blk)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar state
        case findFirstPointCPS points cps of
          Nothing     -> let chain = chainDB cps
                         in return (Nothing, headTip chain)
          Just ipoint -> do
            let !cps' = updateFollower rid ipoint cps
            writeTVar state cps'
            let chain = chainDB cps'
            return (Just ipoint, headTip chain)

    sendNext :: FollowerId
             -> (Tip blk, ChainUpdate blk blk)
             -> ServerStNext (Serialised blk) (Point blk) (Tip blk) m ()
    sendNext r (tip, AddBlock b) = SendMsgRollForward  (Serialised $ toLazyByteString $ encodeNodeToClient codec blockVersion b)             tip (idle' r)
    sendNext r (tip, RollBack p) = SendMsgRollBackward (castPoint p) tip (idle' r)

    newFollower :: m FollowerId
    newFollower = atomically $ do
      cps <- readTVar state
      let (cps', rid) = initFollower genesisPoint cps
      _ <- writeTVar state cps'
      pure rid

    tryReadChainUpdate :: FollowerId
                       -> m (Maybe (Tip blk, ChainUpdate blk blk))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar state
        case followerInstruction rid cps of
          Nothing -> pure Nothing
          Just (u, cps') -> do
            writeTVar state cps'
            let chain = chainDB cps'
            pure $ Just (castTip (headTip chain), u)

    readChainUpdate :: FollowerId -> m (Tip blk, ChainUpdate blk blk)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar state
        case followerInstruction rid cps of
          Nothing -> retry
          Just (u, cps') -> do
            writeTVar state cps'
            let chain = chainDB cps'
            pure (castTip (headTip chain), u)

withSnocket
    :: forall a.
       IOManager
    -> FilePath
    -> (LocalSocket -> LocalSnocket -> IO a)
    -> IO a
withSnocket iocp localDomainSock k =
    bracket localServerInit localServerCleanup localServerBody
  where
    localServerInit :: IO (LocalSocket, LocalSnocket)
    localServerInit = do
      let sn = Snocket.localSnocket iocp localDomainSock
      sd <- Snocket.open sn
              (Snocket.addrFamily sn
                $ Snocket.localAddressFromPath localDomainSock)
      pure (sd, sn)

    -- We close the socket here, even if it was provided for us.
    localServerCleanup :: (LocalSocket, LocalSnocket) -> IO ()
    localServerCleanup (sd, sn) = Snocket.close sn sd

    localServerBody :: (LocalSocket, LocalSnocket) -> IO a
    localServerBody (sd, sn) = do
      Snocket.bind   sn sd (Snocket.localAddressFromPath localDomainSock)
      Snocket.listen sn sd
      k sd sn
