import Test.Tasty
import Test.Tasty.HUnit

import Mamkait.Phonology

main :: IO ()
main = defaultMain suite

exampleString = either (const []) id $ pstring "malEiTRait"

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "ascii to unicode" $
          render exampleString @=? "malëiţřait"
      , testCase "split conjuncts" $
          (map render $ splitConjuncts exampleString) @=? ["m", "a", "l", "ëi", "ţř", "ai", "t"]
      ]
  ]
