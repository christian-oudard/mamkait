import Test.Tasty
import Test.Tasty.HUnit

import Data.List (nub)
import qualified Data.Text as T
import Mamkait.Phonology
import Mamkait.Grammar
import Mamkait.Phonotaxis

main :: IO ()
main = defaultMain suite

exampleAscii, exampleUnicode, exampleUnicodeNFC :: T.Text
exampleAscii = "malEiTRait"
exampleUnicode = "male\x308it\x327r\x30C\&ait"
exampleUnicodeNFC = "mal\235i\355\345ait"

suite :: TestTree
suite = testGroup "test suite" [
    testGroup "unittests"
      [ testCase "ascii to unicode conversions" $ do
          (toAscii . fromAscii) exampleAscii @?= exampleAscii
          (toUnicode . fromAscii) exampleAscii @?= exampleUnicode
          (toAscii . fromUnicode) exampleUnicode @?= exampleAscii
          (toUnicode . fromUnicode) exampleUnicode @?= exampleUnicode
          (toUnicode . fromUnicode) exampleUnicodeNFC @?= exampleUnicode
      , testCase "conversion error, unknown character ignored" $
          (toUnicode . fromAscii) "m2al" @?= "mal"
      , testCase "split conjuncts" $ do
          splitConjunctsAscii exampleAscii @?= ["m", "a", "l", "Ei", "TR", "ai", "t"]
          splitConjunctsUnicode exampleUnicode @?= ["m", "a", "l", "e\x308i", "t\x327r\x30C", "ai", "t"]
      , testCase "syllable stress" $ do
          (getStresses . conjunctsFromAscii) "ma;lalai" @?= [True, False, False]
          (getStresses . conjunctsFromAscii) "malalai;" @?= [False, False, True]
          (getStresses . conjunctsFromAscii) "mala;lai" @?= [False, True, False]
          (getStresses . conjunctsFromAscii) "malalai" @?= [False, True, False]
      , testCase "lexing and concatenating conjuncts" $ do
          (conjunctsToUnicode . conjunctsFromAscii) exampleAscii @?= "male\x308it\x327r\x30C\&ait"
          (conjunctsToUnicode . conjunctsFromAscii) "CalO;r" @?= "c\780alo\770r"
          (conjunctsToAscii . conjunctsFromUnicode) "\269al\244r" @?= "CalO;r"
          (conjunctsToAscii . conjunctsFromUnicode) "c\780alo\770r" @?= "CalO;r"
          (conjunctsToUnicodeHyphenated . conjunctsFromAscii) exampleAscii @?= "m-a-l-e\x308i-t\x327r\x30C-ai-t"
          (conjunctsToUnicode . conjunctsFromAscii) "malalai;" @?= "malala\x301i"
          (conjunctsToUnicode . conjunctsFromAscii) "mala;lai" @?= "malalai"
          (conjunctsToUnicode . conjunctsFromAscii) "ma;lalai" @?= "ma\x301lalai"
          (conjunctsToUnicode . conjunctsFromAscii) "gzz" @?= "gzz"
          (conjunctsToUnicode . conjunctsFromAscii) "ca" @?= "ca"
          (conjunctsToUnicode . conjunctsFromAscii) "mal;" @?= "mal"
      , testCase "allomorphic substitution" $ do
          substituteAllomorphic "tsy" @?= "j"
      , testCase "C_A r/v substitution" $
          substituteAllomorphic (constructCa (DFC, DEL, CSL, P, NRM)) @?= "Rkv"
      , testCase "C_A non-ambiguity" $
          length (nub caClusters) @?= (length caClusters)
      , testCase "C_A phonotaxis" $
          assert $ all (==True) $ map (permissible . fromAscii) caClusters
      ]
  ]

caClusters :: [T.Text]
caClusters = map (substituteAllomorphic . constructCa) allSlotVI
