import Test.Tasty
import Test.Tasty.HUnit

import Mamkait.Phonology

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "ascii to unicode" $
          let ps = pstring "malEiTRait"
          in either (const "") render ps @=? "malëiţřait"
      ]
  ]
