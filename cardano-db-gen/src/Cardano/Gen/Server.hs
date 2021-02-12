{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Cardano.Gen.Server
  ( runLocalServer
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forever, void)
import           Control.Monad.Class.MonadSTM.Strict

import           Control.Tracer (traceWith, nullTracer)

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)

import qualified Network.Socket as Socket
import           Network.TypedProtocol.Core (Peer (..))

import           Ouroboros.Consensus.Network.NodeToClient (Apps (..), DefaultCodecs, Codecs' (..), Handlers (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import           Ouroboros.Consensus.Node (ConnectionId, LowLevelRunNodeArgs (..), NodeKernel,
                   stdVersionDataNTC, stdVersionDataNTN)

import           Ouroboros.Consensus.Block (CodecConfig (..))
import           Ouroboros.Consensus.Config (configCodec)
import           Ouroboros.Consensus.Node.DbLock ()
import           Ouroboros.Consensus.Node.DbMarker ()
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Node.InitStorage ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion, supportedNodeToClientVersions,
                   supportedNodeToNodeVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo ()
import           Ouroboros.Consensus.Node.Recovery ()
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.Node.Tracers ()
import           Ouroboros.Consensus.NodeKernel (NodeKernelArgs, getPeersFromCurrentLedgerAfterSlot)
import           Ouroboros.Consensus.Util.Args ()
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)


import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.Codec (DeserialiseFailure)
import           Ouroboros.Network.Diffusion (DiffusionApplications (..), DiffusionArguments (..),
                   DiffusionInitializationTracer (..), DiffusionTracers (..),
                   LedgerPeersConsensusInterface (..))
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.ErrorPolicy (ErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Mux (OuroborosApplication, MuxMode (..))
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (NodeToClientVersion, NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..), NodeToNodeVersionData (..),
                   RemoteAddress, Versions)
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (NetworkMutableState, NetworkServerTracers (..))
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.TxSubmission.Server

import           Cardano.Gen.ChainSync

runLocalServer
    :: forall blk.
       ( ShowQuery (Query blk)
       , ShowProxy blk
       , ShowProxy (ApplyTxErr blk)
       , ShowProxy (GenTx blk)
       , ShowProxy (Query blk)
       , SerialiseNodeToClientConstraints blk
       )
    => NetworkMagic
    -> FilePath
    -> Map NodeToClientVersion (BlockNodeToClientVersion blk)
    -> StrictTVar IO (ChainProducerState blk)
    -> IO ()
runLocalServer networkMagic localAddress nodeToClientVersions chainvar =
    withIOManager $ \ iom ->
      void $ withSnocket iom localAddress $ \localSocket localSnocket -> do
        networkState <- NodeToClient.newNetworkMutableState
        NodeToClient.withServer
          localSnocket
          NodeToClient.nullNetworkServerTracers -- TODO: some tracing might be useful.
          networkState
          localSocket
          versions
          NodeToClient.networkErrorPolicies

  where
    versions :: Versions NodeToClientVersion
                         NodeToClientVersionData
                         (OuroborosApplication ResponderMode LocalAddress ByteString IO Void ())
    versions = combineVersions
            [ simpleSingletonVersions
                  version
                  (NodeToClientVersionData networkMagic)
                  (NTC.responder version
                    $ mkApps (NTC.defaultCodecs codecConfig blockVersion version))
            | (version, blockVersion) <- Map.toList nodeToClientVersions
            ]

    codecConfig :: CodecConfig blk
    codecConfig = configCodec undefined -- topConfig

    mkApps :: DefaultCodecs blk IO
           -> NTC.Apps IO (ConnectionId addrNTC) ByteString ByteString ByteString ()
    mkApps Codecs {..}  = Apps {..}
      where
        aChainSyncServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aChainSyncServer them channel =
          runPeer
            nullTracer -- TODO add a tracer!
            cChainSyncCodec
            channel
            $ chainSyncServerPeer
            $ chainSyncServer () chainvar

        aTxSubmissionServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aTxSubmissionServer them channel =
          runPeer
            nullTracer
            cTxSubmissionCodec
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

        aStateQueryServer
          :: localPeer
          -> Channel IO ByteString
          -> IO ((), Maybe ByteString)
        aStateQueryServer them channel =
          runPeer
            nullTracer
            cStateQueryCodec
            channel
            (Effect (forever $ threadDelay 3_600_000_000))


withSnocket
    :: forall a.
       IOManager
    -> FilePath
    -> (LocalSocket -> LocalSnocket -> IO a)
    -> IO a
withSnocket iocp localAddress k =
    bracket localServerInit localServerCleanup localServerBody
  where
    localServerInit :: IO (LocalSocket, LocalSnocket)
    localServerInit = do
      let sn = Snocket.localSnocket iocp localAddress
      sd <- Snocket.open sn
              (Snocket.addrFamily sn
                $ Snocket.localAddressFromPath localAddress)
      pure (sd, sn)

    -- We close the socket here, even if it was provided for us.
    localServerCleanup :: (LocalSocket, LocalSnocket) -> IO ()
    localServerCleanup (sd, sn) = Snocket.close sn sd

    localServerBody :: (LocalSocket, LocalSnocket) -> IO a
    localServerBody (sd, sn) = do
      Snocket.bind   sn sd (Snocket.localAddressFromPath localAddress)
      Snocket.listen sn sd
      k sd sn
