import Test.Tasty
import Test.Tasty.HUnit

import Quijada.Phonology

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "round trip conversion" $
          let word = "malëiţřait"
          in (maybe "" render (pstring word)) @=? word
      ]
  ]
