{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Gen.Server
  ( runLocalServer
  , runLocalServerWithManager
  ) where

import           Control.Exception (bracket)
import           Control.Monad (void)

import           Control.Tracer (traceWith)

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))

import qualified Network.Socket as Socket

import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import           Ouroboros.Consensus.Node (ConnectionId, LowLevelRunNodeArgs (..), NodeKernel,
                   RunNode, stdVersionDataNTC, stdVersionDataNTN)

import           Ouroboros.Consensus.Block (CodecConfig (..))
import           Ouroboros.Consensus.Config (TopLevelConfig, configCodec)
import           Ouroboros.Consensus.Node.DbLock ()
import           Ouroboros.Consensus.Node.DbMarker ()
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Node.InitStorage ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion, supportedNodeToClientVersions,
                   supportedNodeToNodeVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo ()
import           Ouroboros.Consensus.Node.Recovery ()
import           Ouroboros.Consensus.Node.Run ()
import           Ouroboros.Consensus.Node.Tracers ()
import           Ouroboros.Consensus.NodeKernel (NodeKernelArgs, getPeersFromCurrentLedgerAfterSlot)
import           Ouroboros.Consensus.Util.Args ()


import           Ouroboros.Network.Codec (DeserialiseFailure)
import           Ouroboros.Network.Diffusion (DiffusionApplications (..), DiffusionArguments (..),
                   DiffusionInitializationTracer (..), DiffusionTracers (..),
                   LedgerPeersConsensusInterface (..))
import           Ouroboros.Network.ErrorPolicy (ErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (NodeToClientVersion, NodeToClientVersionData)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..), NodeToNodeVersionData,
                   RemoteAddress)
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (NetworkMutableState, NetworkServerTracers (..))

runLocalServer
    :: forall blk. RunNode blk
    => DiffusionTracers -> DiffusionArguments -> NetworkMagic
    -> NetworkMutableState LocalAddress -> TopLevelConfig blk
    -> IO ()
runLocalServer tracers difArgs networkMagic networkLocalState topConfig =
    withIOManager $ \ iom ->
      runLocalServerWithManager iom tracers difArgs applications networkLocalState
  where
    -- Holy shit this thing is a endless rabbit hole of stupidity.
    applications :: DiffusionApplications RemoteAddress LocalAddress NodeToNodeVersionData NodeToClientVersionData IO
    applications =
      mkDiffusionApplications
        NodeToNode.defaultMiniProtocolParameters
        two
        three

    mkDiffusionApplications
      :: MiniProtocolParameters
      -> ( BlockNodeToNodeVersion blk
            -> NTN.Apps IO (ConnectionId addrNTN) ByteString ByteString ByteString ByteString ByteString ()
            )
      -> ( BlockNodeToClientVersion blk
            -> NodeToClientVersion
            -> NTC.Apps IO (ConnectionId addrNTC) ByteString ByteString ByteString ()
            )
      -> NodeKernel IO (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> DiffusionApplications
           addrNTN addrNTC
           NodeToNode.NodeToNodeVersionData NodeToClient.NodeToClientVersionData
           IO
    mkDiffusionApplications miniProtocolParams ntnApps ntcApps kernel =
      DiffusionApplications
        { daResponderApplication =
            combineVersions
              [ simpleSingletonVersions
                    version
                    (llrnVersionDataNTN llArgs)
                    (NTN.responder miniProtocolParams version $ ntnApps blockVersion)
              | (version, blockVersion) <- Map.toList (llrnNodeToNodeVersions llArgs)
              ]
        , daInitiatorApplication =
            combineVersions
              [ simpleSingletonVersions
                    version
                    (llrnVersionDataNTN llArgs)
                    (NTN.initiator miniProtocolParams version $ ntnApps blockVersion)
              | (version, blockVersion) <- Map.toList (llrnNodeToNodeVersions llArgs)
              ]
        , daLocalResponderApplication =
            combineVersions
              [ simpleSingletonVersions
                    version
                    (llrnVersionDataNTC llArgs)
                    (NTC.responder version $ ntcApps blockVersion version)
              | (version, blockVersion) <- Map.toList (llrnNodeToClientVersions llArgs)
              ]
        , daErrorPolicies = consensusErrorPolicy (Proxy @blk)
        , daLedgerPeersCtx = LedgerPeersConsensusInterface (getPeersFromCurrentLedgerAfterSlot kernel)
        }

    codecConfig :: CodecConfig blk
    codecConfig = configCodec topConfig

    mkNodeToNodeApps
        :: NodeKernelArgs IO (ConnectionId addrNTN) (ConnectionId addrNTC) blk
        -> NodeKernel     IO (ConnectionId addrNTN) (ConnectionId addrNTC) blk
        -> BlockNodeToNodeVersion blk
        -> NTN.Apps IO (ConnectionId addrNTN) ByteString ByteString ByteString ByteString ByteString ()
    mkNodeToNodeApps nodeKernelArgs nodeKernel version =
        NTN.mkApps
          nodeKernel
          rnTraceNTN
          (NTN.defaultCodecs codecConfig version)
          (llrnChainSyncTimeout llArgs)
          (NTN.mkHandlers nodeKernelArgs nodeKernel)

    mkNodeToClientApps
        :: NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
        -> NodeKernel     m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
        -> BlockNodeToClientVersion blk
        -> NodeToClientVersion
        -> NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ()
    mkNodeToClientApps nodeKernelArgs nodeKernel blockVersion networkVersion =
        NTC.mkApps
          nodeKernel
          rnTraceNTC
          (NTC.defaultCodecs codecConfig blockVersion networkVersion)
          (NTC.mkHandlers nodeKernelArgs nodeKernel)

    rnTraceNTN :: NTN.Tracers m (ConnectionId addrNTN) blk DeserialiseFailure
    rnTraceNTN = error "Cardano.Gen.Server.runLocalServer: rnTraceNTN"

    rnTraceNTC :: NTC.Tracers m (ConnectionId addrNTC) blk DeserialiseFailure
    rnTraceNTC = error "Cardano.Gen.Server.runLocalServer: rnTraceNTC"

    llArgs :: LowLevelRunNodeArgs IO addrNTN addrNTC NodeToNode.NodeToNodeVersionData NodeToClient.NodeToClientVersionData blk
    llArgs =
      LowLevelRunNodeArgs
        { llrnWithCheckedDB = error "Cardano.Gen.Server.runLocalServer: llrnWithCheckedDB"
        , llrnChainDbArgsDefaults = error "Cardano.Gen.Server.runLocalServer: llrnChainDbArgsDefaults"
        , llrnCustomiseChainDbArgs = error "Cardano.Gen.Server.runLocalServer: llrnCustomiseChainDbArgs"
        , llrnCustomiseNodeKernelArgs = error "Cardano.Gen.Server.runLocalServer: llrnCustomiseNodeKernelArgs"
        , llrnBfcSalt = error "Cardano.Gen.Server.runLocalServer: llrnBfcSalt"
        , llrnKeepAliveRng = error "Cardano.Gen.Server.runLocalServer: llrnKeepAliveRng"
        , llrnCustomiseHardForkBlockchainTimeArgs = error "Cardano.Gen.Server.runLocalServer: llrnCustomiseHardForkBlockchainTimeArgs"
        , llrnChainSyncTimeout = error "Cardano.Gen.Server.runLocalServer: llrnChainSyncTimeout"
        , llrnRunDataDiffusion = error "Cardano.Gen.Server.runLocalServer: llrnRunDataDiffusion"
        , llrnMaxClockSkew = error "Cardano.Gen.Server.runLocalServer: llrnMaxClockSkew"

        , llrnVersionDataNTC = NodeToClient.NodeToClientVersionData networkMagic
        , llrnVersionDataNTN = NodeToNode.NodeToNodeVersionData networkMagic (daDiffusionMode difArgs)
        , llrnNodeToNodeVersions = supportedNodeToNodeVersions (Proxy @blk)
        , llrnNodeToClientVersions = supportedNodeToClientVersions (Proxy @blk)
        }


runLocalServerWithManager
    :: IOManager -> DiffusionTracers -> DiffusionArguments
    -> DiffusionApplications RemoteAddress LocalAddress NodeToNodeVersionData NodeToClientVersionData IO
    -> NetworkMutableState LocalAddress
    -> IO ()
runLocalServerWithManager iocp tracers difArgs applications networkLocalState =
    bracket localServerInit localServerCleanup localServerBody
  where
    localServerInit :: IO (LocalSocket, LocalSnocket)
    localServerInit =
      case daLocalAddress difArgs of
        Left sd -> do
          a <- Socket.getSocketName sd
          case a of
            Socket.SockAddrUnix path -> do
              traceInitializer $ UsingSystemdSocket path
              pure (LocalSocket sd, Snocket.localSnocket iocp path)
            unsupportedAddr -> do
              traceInitializer $ UnsupportedLocalSystemdSocket unsupportedAddr
              error "UnsupportedLocalSocketType"
        Right addr -> do
          let sn = Snocket.localSnocket iocp addr
          traceInitializer $ CreateSystemdSocketForSnocketPath addr
          sd <- Snocket.open sn (Snocket.addrFamily sn $ Snocket.localAddressFromPath addr)
          traceInitializer $ CreatedLocalSocket addr
          pure (sd, sn)

    -- We close the socket here, even if it was provided for us.
    localServerCleanup :: (LocalSocket, LocalSnocket) -> IO ()
    localServerCleanup (sd, sn) = Snocket.close sn sd

    localServerBody :: (LocalSocket, LocalSnocket) -> IO ()
    localServerBody (sd, sn) = do
      case daLocalAddress difArgs of
        -- If a socket was provided it should be ready to accept
        Left _ -> pure ()
        Right path -> do
          Snocket.localSocketFileDescriptor sd
            >>= traceInitializer . ConfiguringLocalSocket path


          Snocket.bind sn sd $ Snocket.localAddressFromPath path
          Snocket.localSocketFileDescriptor sd
            >>= traceInitializer . ListeningLocalSocket path

          Snocket.listen sn sd

          Snocket.localSocketFileDescriptor sd
            >>= traceInitializer . LocalSocketUp path

          Snocket.getLocalAddr sn sd
            >>= traceInitializer . RunLocalServer

      void $ NodeToClient.withServer
                sn
                (NetworkServerTracers
                    (dtMuxLocalTracer tracers)
                    (dtHandshakeLocalTracer tracers)
                    (dtLocalErrorPolicyTracer tracers)
                    (dtAcceptPolicyTracer tracers)
                    )
                networkLocalState
                sd
                (daLocalResponderApplication applications)
                localErrorPolicy

    traceInitializer :: DiffusionInitializationTracer -> IO ()
    traceInitializer = traceWith (dtDiffusionInitializationTracer tracers)

    localErrorPolicy :: ErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies applications
