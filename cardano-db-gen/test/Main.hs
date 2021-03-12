import Test.Tasty
import Test.QSM
import Prelude

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  cfg <- mkConfig params
  return $
    testGroup
      "cardano-db-gen" [qsmTests cfg]
