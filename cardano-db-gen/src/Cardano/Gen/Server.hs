{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Gen.Server
  ( runLocalServer
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forever, void)
import           Control.Monad.Class.MonadSTM.Strict

import           Control.Tracer (nullTracer)

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import           Network.TypedProtocol.Core (Peer (..))

import           Ouroboros.Consensus.Block (CodecConfig, HasHeader)
import           Ouroboros.Consensus.Config (TopLevelConfig, configCodec)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.Network.NodeToClient (Apps (..), Codecs' (..), DefaultCodecs)
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import           Ouroboros.Consensus.Node (ConnectionId)
import           Ouroboros.Consensus.Node.DbLock ()
import           Ouroboros.Consensus.Node.DbMarker ()
import           Ouroboros.Consensus.Node.InitStorage ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.ProtocolInfo ()
import           Ouroboros.Consensus.Node.Recovery ()
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.Node.Tracers ()
import           Ouroboros.Consensus.Util.Args ()

import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MuxMode (..), OuroborosApplication)
import           Ouroboros.Network.NodeToClient (NodeToClientVersion, NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (Versions)
import           Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import           Ouroboros.Network.Protocol.ChainSync.Server (chainSyncServerPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)

import           Cardano.Gen.ChainSync

runLocalServer
    :: forall blk.
       ( HasHeader blk
       , ShowQuery (Query blk)
       , ShowProxy blk
       , ShowProxy (ApplyTxErr blk)
       , ShowProxy (GenTx blk)
       , ShowProxy (Query blk)
       , SerialiseNodeToClientConstraints blk
       )
    => NetworkMagic
    -> TopLevelConfig blk
    -> FilePath
    -> Map NodeToClientVersion (BlockNodeToClientVersion blk)
    -> StrictTVar IO (ChainProducerState blk)
    -> IO ()
runLocalServer networkMagic config localDomainSock nodeToClientVersions chainvar =
    withIOManager $ \ iom ->
      void $ withSnocket iom localDomainSock $ \ localSocket localSnocket -> do
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
                         (OuroborosApplication 'ResponderMode LocalAddress ByteString IO Void ())
    versions = combineVersions
            [ simpleSingletonVersions
                  version
                  (NodeToClientVersionData networkMagic)
                  (NTC.responder version
                    $ mkApps version blockVersion (NTC.defaultCodecs codecConfig blockVersion version))
            | (version, blockVersion) <- Map.toList nodeToClientVersions
            ]

    codecConfig :: CodecConfig blk
    codecConfig = configCodec config

    mkApps :: NodeToClientVersion -> BlockNodeToClientVersion blk -> DefaultCodecs blk IO
           -> NTC.Apps IO (ConnectionId addrNTC) ByteString ByteString ByteString ()
    mkApps _version blockVersion Codecs {..}  = Apps {..}
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
            $ chainSyncServer chainvar codecConfig blockVersion

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
