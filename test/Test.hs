import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Mamkait.Phonology

main :: IO ()
main = defaultMain suite

exampleAscii, exampleUnicode, exampleUnicodeNFC :: T.Text
exampleAscii = "malEiTRait"
exampleUnicode = "male\x308it\x327r\x30C\&ait"
exampleUnicodeNFC = "mal\235i\355\345ait"

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "ascii to ascii" $
           (toAscii . fromAscii) exampleAscii @?= exampleAscii
      , testCase "ascii to unicode" $
           (toUnicode . fromAscii) exampleAscii @?= exampleUnicode
      , testCase "unicode to ascii" $
           (toAscii . fromUnicode) exampleUnicode @?= exampleAscii
      , testCase "unicode to unicode" $
           (toUnicode . fromUnicode) exampleUnicode @?= exampleUnicode
      , testCase "unicode to unicode, fix unicode normalization" $
           (toUnicode . fromUnicode) exampleUnicodeNFC @?= exampleUnicode
      , testCase "error, unknown character ignored" $
           (toUnicode . fromAscii) "m2al" @?= "mal"
      , testCase "split conjuncts" $
          splitConjuncts exampleAscii @?= ["m", "a", "l", "Ei", "TR", "ai", "t"]
      , testCase "syllable stress initial" $
          (getStresses . conjunctsFromAscii) "ma;lalai" @?= [True, False, False]
      , testCase "syllable stress final" $
          (getStresses . conjunctsFromAscii) "malalai;" @?= [False, False, True]
      , testCase "syllable stress penultimate, explicit" $
          (getStresses . conjunctsFromAscii) "mala;lai" @?= [False, True, False]
      , testCase "syllable stress penultimate, by default" $
          (getStresses . conjunctsFromAscii) "malalai" @?= [False, True, False]
      , testCase "ascii to unicode via conjuncts" $
          (conjunctsToUnicode . conjunctsFromAscii) exampleAscii @?= "male\x308it\x327r\x30C\&ait"
      , testCase "conjucts to unicode, hyphenated" $
          (conjunctsToUnicodeHyphenated . conjunctsFromAscii) exampleAscii @?= "m-a-l-e\x308i-t\x327r\x30C-ai-t"
      , testCase "conjuncts to unicode, ultimate stress" $
          (conjunctsToUnicode . conjunctsFromAscii) "malalai;" @?= "malala\x301i"
      , testCase "conjuncts to unicode, penultimate stress (not shown)" $
          (conjunctsToUnicode . conjunctsFromAscii) "mala;lai" @?= "malalai"
      , testCase "conjuncts to unicode, antepenultimate stress" $
          (conjunctsToUnicode . conjunctsFromAscii) "ma;lalai" @?= "ma\x301lalai"
      , testCase "conjuncts to unicode, small word, only consonants" $
          (conjunctsToUnicode . conjunctsFromAscii) "gzz" @?= "gzz"
      , testCase "conjuncts to unicode, small word, ending in a vowel" $
          (conjunctsToUnicode . conjunctsFromAscii) "ca" @?= "ca"
      , testCase "conjunct error, stress on consonant ignored" $
          (conjunctsToUnicode . conjunctsFromAscii) "mal;" @?= "mal"
      ]
  ]
