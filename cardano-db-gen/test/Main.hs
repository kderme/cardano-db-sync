import Test.Tasty
import Test.QSM
import Prelude

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  return $
    testGroup
      "cardano-db-gen" [qsmTests]
