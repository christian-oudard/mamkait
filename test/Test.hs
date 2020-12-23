import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Mamkait.Phonology

main :: IO ()
main = defaultMain suite

exampleString :: T.Text
exampleString = "malEiTRait"

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "ascii to unicode" $
           (renderPString . pstring) exampleString @?= "male\x308it\x327r\x30C\&ait"
      , testCase "split conjuncts" $
          splitConjuncts exampleString @?= ["m", "a", "l", "Ei", "TR", "ai", "t"]
      , testCase "syllable stress initial" $
          (getStresses . lexConjuncts) "ma;lala" @?= [True, False, False]
      , testCase "syllable stress final" $
          (getStresses . lexConjuncts) "malala;" @?= [False, False, True]
      , testCase "syllable stress penultimate, explicit" $
          (getStresses . lexConjuncts) "mala;la" @?= [False, True, False]
      , testCase "syllable stress penultimate, by default" $
          (getStresses . lexConjuncts) "malala" @?= [False, True, False]
      , testCase "lex and render" $
          (render . lexConjuncts) exampleString @?= "male\x308it\x327r\x30C\&ait"
      , testCase "lex and render, hyphenated" $
          (renderHyphenated . lexConjuncts) exampleString @?= "m-a-l-e\x308i-t\x327r\x30C-ai-t"
      , testCase "lex and render, ultimate stress" $
          (render . lexConjuncts) "malala;" @?= "malala\x301"
      , testCase "lex and render, penultimate stress (not shown)" $
          (render . lexConjuncts) "mala;la" @?= "malala"
      , testCase "lex and render, antepenultimate stress" $
          (render . lexConjuncts) "ma;lala" @?= "ma\x301lala"
      , testCase "lex and render, small word" $
          (render . lexConjuncts) "gzz" @?= "gzz"
      , testCase "lex error, stress on consonant ignored" $
          (render . lexConjuncts) "mal;" @?= "mal"
      , testCase "pstring error, unknown character ignored" $
           (renderPString . pstring) "m2al" @?= "mal"
      ]
  ]
