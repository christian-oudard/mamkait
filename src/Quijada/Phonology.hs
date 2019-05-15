module Quijada.Phonology where

import Data.Maybe (fromJust)


--- Phonemic Inventory ---

letterChart :: [(String, Char, Phoneme)]
-- 1. ASCII representation
-- 2. Unicode display character
-- 3. Phoneme classification
letterChart =
  -- Consonant
  [ ("p",  'p', CT Unvoiced Labial Stop)
  , ("b",  'b', CT Voiced Labial Stop)
  , ("t",  't', CT Unvoiced ApicoDental Stop)
  , ("d",  'd', CT Voiced ApicoDental Stop)
  , ("k",  'k', CT Unvoiced Velar Stop)
  , ("g",  'g', CT Voiced Velar Stop)
  , ("'",  '’', CT Unvoiced Glottal Stop)
  , ("f",  'f', CT Unvoiced LabioDental Fricative)
  , ("v",  'v', CT Voiced LabioDental Fricative)
  , ("t,", 'ţ', CT Unvoiced InterDental Fricative)
  , ("d,", 'ḑ', CT Voiced InterDental Fricative)
  , ("s",  's', CT Unvoiced ApicoAlveolar Fricative)
  , ("z",  'z', CT Voiced ApicoAlveolar Fricative)
  , ("s/", 'š', CT Unvoiced AlveoloPalatal Fricative)
  , ("z/", 'ž', CT Voiced AlveoloPalatal Fricative)
  , ("x",  'x', CT Unvoiced Uvular Fricative)
  , ("h",  'h', CT Unvoiced Glottal Fricative)
  , ("l,", 'ļ', CT Unvoiced Lateral Fricative)
  , ("c",  'c', CT Unvoiced ApicoAlveolar Affricative)
  , ("z.", 'ż', CT Voiced ApicoAlveolar Affricative)
  , ("c/", 'č', CT Unvoiced AlveoloPalatal Affricative)
  , ("j",  'j', CT Voiced AlveoloPalatal Affricative)
  , ("m",  'm', CT Voiced Labial Nasal)
  , ("n",  'n', CT Voiced ApicoDental Nasal)
  , ("n/", 'ň', CT Voiced Velar Nasal)
  , ("r",  'r', CT Voiced AlveolarRetroflex Tap)
  , ("l",  'l', CT Voiced Lateral Liquid)
  , ("w",  'w', CT Voiced LabioVelar Approximant)
  , ("y",  'y', CT Voiced Palatal Approximant)
  , ("r/", 'ř', CT Voiced Uvular Approximant)
  -- Vowels
  , ("i",  'i', VT Unrounded Front High)
  , ("u;", 'ü', VT Rounded Front High)
  , ("u",  'u', VT Rounded Back High)
  , ("e",  'e', VT Unrounded Front Mid)
  , ("o;", 'ö', VT Rounded Front Mid)
  , ("e;", 'ë', VT Unrounded Back Mid)
  , ("o",  'o', VT Rounded Back Mid)
  , ("a",  'a', VT Unrounded Central Low)
  , ("a;", 'ä', VT Unrounded Back Low)
  ]

data Phoneme
  = CT CVoicedness CPlace CManner
  | VT VRoundedness VPlace VHeight
  deriving (Eq, Show)

data CVoicedness = Voiced | Unvoiced
  deriving (Eq, Show)
data CPlace
  = Labial | LabioDental | LabioVelar | ApicoDental | InterDental | ApicoAlveolar
  | Alveolar | AlveolarRetroflex | AlveoloPalatal | Palatal | Velar | Uvular | Glottal | Lateral
  deriving (Eq, Show)
data CManner = Stop | Fricative | Affricative | Nasal | Tap | Liquid | Approximant
  deriving (Eq, Show)

data VRoundedness = Rounded | Unrounded
  deriving (Eq, Show)
data VPlace = Front | Central | Back
  deriving (Eq, Show)
data VHeight = High | Mid | Low
  deriving (Eq, Show)

letters :: [String]
letters = map (\(s, _, _) -> s) letterChart

phoneme :: String -> Maybe Phoneme
phoneme s = lookup s $ map (\(k, _, p) -> (k, p)) $ letterChart

chr :: String -> Maybe Char
chr " " = Just ' '
chr s = lookup s $ map (\(k, c, _) -> (k, c)) $ letterChart

consonants :: [String]
consonants = filter isConsonant letters
  where isConsonant s = case phoneme s of Just (CT _ _ _) -> True; _ -> False

vowels :: [String]
vowels = filter isVowel letters
  where isVowel s = case phoneme s of Just (VT _ _ _) -> True; _ -> False

display :: String -> String
display s = concatMap chr' $ tokens s
  where chr' s' = case chr s' of Just c -> [c]; Nothing -> s'

tokens :: String -> [String]
tokens (a:b:xs) =
  if [a, b] `elem` letters
  then [a, b]:tokens xs
  else [a]:tokens (b:xs)
tokens [a] = [[a]]
tokens [] = []


--- Phonotaxis ---

isLegalPair :: Phoneme -> Phoneme -> Bool
-- different voicedness, same place, same manner
isLegalPair (CT Unvoiced m1 p1) (CT Voiced m2 p2)
  | m1 == m2 && p1 == p2  = False
  | otherwise  = True
isLegalPair _ _ = True
-- STUB


--- Pitch ---

data Pitch = MidTone | RisingTone | FallingTone | RiseFallTone | FallRiseTone | LowTone
  deriving (Eq, Show)
