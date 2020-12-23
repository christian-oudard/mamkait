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
          (getStresses . lexConjuncts) "ma;lalai" @?= [True, False, False]
      , testCase "syllable stress final" $
          (getStresses . lexConjuncts) "malalai;" @?= [False, False, True]
      , testCase "syllable stress penultimate, explicit" $
          (getStresses . lexConjuncts) "mala;lai" @?= [False, True, False]
      , testCase "syllable stress penultimate, by default" $
          (getStresses . lexConjuncts) "malalai" @?= [False, True, False]
      , testCase "lex and render" $
          (render . lexConjuncts) exampleAscii @?= "male\x308it\x327r\x30C\&ait"
      , testCase "lex and render, hyphenated" $
          (renderHyphenated . lexConjuncts) exampleAscii @?= "m-a-l-e\x308i-t\x327r\x30C-ai-t"
      , testCase "lex and render, ultimate stress" $
          (render . lexConjuncts) "malalai;" @?= "malala\x301i"
      , testCase "lex and render, penultimate stress (not shown)" $
          (render . lexConjuncts) "mala;lai" @?= "malalai"
      , testCase "lex and render, antepenultimate stress" $
          (render . lexConjuncts) "ma;lalai" @?= "ma\x301lalai"
      , testCase "lex and render, small word" $
          (render . lexConjuncts) "gzz" @?= "gzz"
      , testCase "lex error, stress on consonant ignored" $
          (render . lexConjuncts) "mal;" @?= "mal"
      ]
  ]
