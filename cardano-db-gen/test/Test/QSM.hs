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
  ( qsmTests,
  )
where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.Kind (Type)
import           GHC.Generics

import           Ouroboros.Network.Block
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
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

import qualified Cardano.Mock.Chain as Chain
import           Cardano.Mock.Server
import           Cardano.Mock.Types

import           Cardano.Sync.Config
import           Cardano.Sync.Config.Types

import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr


qsmTests :: TestTree
qsmTests =
  testGroup
    "qsm-tests"
        [ testProperty "qsm" prop_1]


data Model c (r :: Type -> Type)
  = Model
      { chain :: Maybe (Chain c)
      }
  deriving (Generic)

deriving instance (Show (Block c), Show (ExtLedgerState c)) => Show (Model c r)
deriving instance (Eq (Block c), Eq (ExtLedgerState c)) => Eq (Model c r)
deriving instance Generic (Chain c)
deriving instance (ToExpr (ExtLedgerState c), ToExpr (Block c)) => ToExpr (Chain c)
deriving instance (ToExpr (ExtLedgerState c), ToExpr (Block c)) => ToExpr (Model c Concrete)

initModel :: Model c r
initModel = Model Nothing

data Command c (r :: Type -> Type)
  = AddGenesis (ExtLedgerState c)
  | AddBlock (Block c)
  | RollBack Int (Block c)
  deriving (Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving instance (Show (Block c), CardanoHardForkConstraints c) => Show (Command c r)
deriving instance (Eq (Block c), CardanoHardForkConstraints c) => Eq (Command c r)

newtype Response c (r :: Type -> Type)
  = Response
      {getResponse :: Either Error Success}
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Foldable)

data Success = Unit
  deriving (Eq, Show, Generic, ToExpr)

data Error = Error
  deriving (Eq, Show, Generic, ToExpr)

type Config = ()

transition :: Config -> Model c r -> Command c r -> Response c r -> Model c r
transition cfg Model {..} cmd _resp = undefined

precondition :: Model c Symbolic -> Command c Symbolic -> Logic
precondition Model {..} _ = Top

postcondition :: Config -> Model c Concrete -> Command c Concrete -> Response c Concrete -> Logic
postcondition cfg m@Model {..} cmd resp =
  resp .== toMock cfg m cmd

toMock :: Config -> Model c r -> Command c r -> Response c r
toMock cfg Model {..} cmd = Response $ Right Unit

generator :: Model c Symbolic -> Maybe (Gen (Command c Symbolic))
generator Model {..} = Nothing

shrinker :: Model c Symbolic -> Command c Symbolic -> [Command c Symbolic]
shrinker _ _ = []

semantics :: Config -> ServerHandle m blk -> Command c Concrete -> IO (Response c Concrete)
semantics cfg _ _ = undefined

mock :: Config -> Model c Symbolic -> Command c Symbolic -> GenSym (Response c Symbolic)
mock cfg m cmd = return $ toMock cfg m cmd

mkSM :: Config -> ServerHandle m blk -> StateMachine (Model c) (Command c) IO (Response c)
mkSM cfg handle =
  StateMachine
    initModel
    (transition cfg)
    precondition
    (postcondition cfg)
    Nothing
    generator
    shrinker
    (semantics cfg handle)
    (mock cfg)
    noCleanup

unusedSM :: c ~ StandardCrypto => Config -> StateMachine (Model c) (Command c) IO (Response c)
unusedSM cfg = mkSM cfg $ error "ServerHandle not used on generation or shrinking"

prop_1 :: Property
prop_1 = noShrinking $ withMaxSuccess 1
  $ forAllCommands (unusedSM ()) Nothing
  $ \cmds -> monadicIO $ do
    mockServer <- liftIO $ forkServerThread @(Block StandardCrypto) (NetworkMagic 42) socketPath
    node <- liftIO $ async $ runDbSyncNode extendedDbSyncNodePlugin params
    liftIO $ link node
    let sm = mkSM () mockServer
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ cancel node
    liftIO $ stopServer mockServer
    prettyCommands sm hist (res === Ok)
  where
    params :: SyncNodeParams
    params = SyncNodeParams
      { enpConfigFile = ConfigFile "testfiles/config.json"
      , enpSocketPath = SocketPath socketPath
      , enpLedgerStateDir = LedgerStateDir "testfiles/ledger-states"
      , enpMigrationDir = MigrationDir "../schema"
      , enpMaybeRollback = Nothing
      }

    socketPath = "testfiles/.socket"

instance ToExpr (ExtLedgerState StandardCrypto) where
  toExpr _ = Lst [] -- TODO
deriving instance Generic (Block StandardCrypto)
instance ToExpr (Block StandardCrypto) where
  toExpr _ = Lst [] -- TODO
deriving instance Generic (ExtLedgerState StandardCrypto)
deriving instance Generic (LedgerState (Block StandardCrypto))
