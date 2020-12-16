import Test.Tasty
import Test.Tasty.HUnit

import Quijada.Phonology

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "unicode characters" $ unicodeChars @=? "pbtdkg’fvţḑszšžxhļcẓčjmnňrlwyřiüueöëoaä"
      , testCase "convert ascii representations" $
          convert (concat asciiReps) @=? unicodeChars
      , testCase "convert word" $ convert "male;it,r/ait" @=? "malëiţřait"
      , testCase "convert word error" $ convert "male:it,r/ait" @=? "male�iţřait"
      , testCase "deconvert unicode chars" $ deconvert unicodeChars @=? concat asciiReps
      ]
  ]
