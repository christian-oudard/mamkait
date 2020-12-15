import Test.Tasty
import Test.Tasty.HUnit

import Quijada.Phonology

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "unicode characters" $ unicodeChars @=? "pbtdkg’fvţḑszšžxhļcẓčjmnňrlwyřiüueöëoaä"
      , testCase "convert" $ convert (concat asciiReps) @=? Just unicodeChars
      , testCase "deconvert" $ deconvert unicodeChars @=? Just (concat asciiReps)
      ]
  ]
