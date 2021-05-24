{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QSM
  ( qsmTests
  , mkConfig
  , params
  )
where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Kind (Type)
import qualified Data.Text as Text
import           GHC.Generics

import           Ouroboros.Network.Block hiding (AddBlock, RollBack)
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Protocol

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO)
import           Test.StateMachine
import           Test.StateMachine.Sequential ()
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.StateMachine.Utils
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.DbSync (runDbSyncNode)
import           Cardano.DbSync.Plugin.Extended
import           Cardano.Sync hiding (Block)
import           Cardano.Sync.Config.Cardano

import qualified Cardano.Mock.Chain as Chain
import           Cardano.Mock.Server
import           Cardano.Mock.Types

import           Cardano.Sync.Config
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Error

import           Data.Functor.Classes
import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr

qsmTests :: (c ~ StandardCrypto, LedgerSupportsProtocol (Block c)) => Config c -> TestTree
qsmTests cfg =
  testGroup
    "qsm-tests"
        [ testProperty "qsm" (prop_1 cfg)]

mkConfig :: SyncNodeParams -> IO (Config StandardCrypto)
mkConfig params = do
  config <- readSyncNodeConfig $ enpConfigFile params
  genCfg <- either (error . Text.unpack . renderSyncNodeError) id <$> (runExceptT $ readCardanoGenesisConfig config)
  return $ Consensus.pInfoConfig $ mkProtocolInfoCardano genCfg

data Model c (r :: Type -> Type)
  = Model
      { chain :: Chain.ChainDB (Block c)
      , mockServerHandle :: Maybe (Reference (Opaque (ServerHandle IO (Block c))) r)
      , dbsyncThread :: Maybe (Reference (Opaque (Async ())) r)
      }
  deriving (Generic)

deriving instance (Show (Block c), Show (ExtLedgerState c), Show1 r) => Show (Model c r)
-- deriving instance (Eq (Chain c)) => Eq (Model c r)
deriving instance Generic (Chain c)
deriving instance Generic (Chain.ChainDB c)
deriving instance (ToExpr (ExtLedgerState c), ToExpr (Block c)) => ToExpr (Chain c)
instance (ToExpr (ExtLedgerState c), ToExpr (Block c)) => ToExpr (Chain.ChainDB (Block c)) where
  toExpr = toExpr . Chain.cchain
deriving instance (ToExpr (ExtLedgerState c), ToExpr (Block c)) => ToExpr (Model c Concrete)

initModel :: Config c -> Model c r
initModel cfg = Model (Chain.initChainDB cfg) Nothing Nothing

data Command c (r :: Type -> Type)
  = RunNode (ExtLedgerState c)
  | RunDBSync
  | AddBlock (Block c)
  | RollBack Int
  deriving (Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving instance (Show (Block c), CardanoHardForkConstraints c) => Show (Command c r)
deriving instance (Eq (Block c), CardanoHardForkConstraints c) => Eq (Command c r)

data Response c (r :: Type -> Type)
  = MockServerHandle (Reference (Opaque (ServerHandle IO (Block c))) r)
  | DBSyncThread (Reference (Opaque (Async ())) r)
  | Unit ()
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Foldable)

data Error = Error
  deriving (Eq, Show, Generic, ToExpr)

transition :: forall c r. LedgerSupportsProtocol (Block c) => Model c r -> Command c r -> Response c r -> Model c r
transition Model {..} cmd resp = case cmd of
  RunMockServer st -> Model (Chain.insertGenesis chain st) mockServerHandle dbsyncThread
  AddBlock blk -> Model (Chain.extendChain @(Block c) undefined blk) mockServerHandle dbsyncThread
  RollBack _ -> error "rollback not supported"

precondition :: Model c Symbolic -> Command c Symbolic -> Logic
precondition Model {..} _ = Top

postcondition :: Config c -> Model c Concrete -> Command c Concrete -> Response c Concrete -> Logic
postcondition cfg m@Model {..} cmd resp =
  resp .== toMock cfg m cmd

toMock :: Config c -> Model c r -> Command c r -> Response c r
toMock cfg Model {..} _cmd = Unit ()

generator :: Model c Symbolic -> Maybe (Gen (Command c Symbolic))
generator Model {..} = case mockServerHandle of
  Nothing -> AddGenesis

shrinker :: Model c Symbolic -> Command c Symbolic -> [Command c Symbolic]
shrinker _ _ = []

semantics :: LedgerSupportsProtocol (Block c) => ServerHandle IO (Block c) -> Command c Concrete -> IO (Response c Concrete)
semantics handle cmd = do
  _ <- atomically $ case cmd of
    AddGenesis st -> addGenesis handle st
    AddBlock blk -> addBlock handle blk
    RollBack _ -> error "not supported"
  return $ Unit ()

mock :: Config c -> Model c Symbolic -> Command c Symbolic -> GenSym (Response c Symbolic)
mock cfg m cmd = return $ toMock cfg m cmd

mkSM :: forall c. LedgerSupportsProtocol (Block c) => Config c -> ServerHandle IO (Block c) -> StateMachine (Model c) (Command c) IO (Response c)
mkSM cfg handle =
  StateMachine
    (initModel cfg)
    transition
    precondition
    (postcondition cfg)
    Nothing
    generator
    shrinker
    (semantics handle)
    (mock cfg)
    noCleanup

unusedSM :: forall c. LedgerSupportsProtocol (Block c) => Config c -> StateMachine (Model c) (Command c) IO (Response c)
unusedSM cfg = mkSM cfg $ error "ServerHandle not used on generation or shrinking"

prop_1 :: forall c. (c ~ StandardCrypto, LedgerSupportsProtocol (Block c), MockServerConstraint (Block c)) => Config c -> Property
prop_1 cfg = verbose $ noShrinking $ withMaxSuccess 1
  $ forAllCommands smc Nothing
  $ \cmds -> monadicIO $ do
    mockServer <- liftIO $ forkServerThread @(Block c) cfg (NetworkMagic 764824073) $ unSocketPath (enpSocketPath params)
    node <- liftIO $ async $ runDbSyncNode nullMetricSetters extendedDbSyncNodePlugin params
    liftIO $ link node
    let sm = mkSM @c cfg mockServer
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ threadDelay 100000000
    liftIO $ cancel node
    liftIO $ stopServer mockServer
    prettyCommands sm hist (res === Ok)
  where
    smc = unusedSM @c cfg

params :: SyncNodeParams
params = SyncNodeParams
  { enpConfigFile = ConfigFile "test/testfiles/config.yaml"
  , enpSocketPath = SocketPath "/home/kostas/programming/cardano-db-sync/cardano-db-gen/test/testfiles/.socket"
  , enpLedgerStateDir = LedgerStateDir "testfiles/ledger-states"
  , enpMigrationDir = MigrationDir "../schema"
  , enpMaybeRollback = Nothing
  }

instance ToExpr (ExtLedgerState StandardCrypto) where
  toExpr _ = Lst [] -- TODO
deriving instance Generic (Block StandardCrypto)
instance ToExpr (Block StandardCrypto) where
  toExpr _ = Lst [] -- TODO
deriving instance Generic (ExtLedgerState StandardCrypto)
deriving instance Generic (LedgerState (Block StandardCrypto))
