import Test.Tasty
import Test.Tasty.HUnit

import Quijada.Phonology

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      []
  ]
