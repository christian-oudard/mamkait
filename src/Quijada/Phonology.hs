module Quijada.Phonology where

data Phoneme
  = C CVoicedness CPlace CManner
  | V VRoundedness VPlace VHeight
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

--- Phonemic Inventory ---

phonemeChart :: [(String, Char, Phoneme)]
-- 1. ASCII representation
-- 2. Unicode display character
-- 3. Phoneme classification
phonemeChart =
  -- Consonant
  [ ("p",  'p', C Unvoiced Labial Stop)
  , ("b",  'b', C Voiced Labial Stop)
  , ("t",  't', C Unvoiced ApicoDental Stop)
  , ("d",  'd', C Voiced ApicoDental Stop)
  , ("k",  'k', C Unvoiced Velar Stop)
  , ("g",  'g', C Voiced Velar Stop)
  , ("'",  '’', C Unvoiced Glottal Stop)
  , ("f",  'f', C Unvoiced LabioDental Fricative)
  , ("v",  'v', C Voiced LabioDental Fricative)
  , ("t,", 'ţ', C Unvoiced InterDental Fricative)
  , ("d,", 'ḑ', C Voiced InterDental Fricative)
  , ("s",  's', C Unvoiced ApicoAlveolar Fricative)
  , ("z",  'z', C Voiced ApicoAlveolar Fricative)
  , ("s/", 'š', C Unvoiced AlveoloPalatal Fricative)
  , ("z/", 'ž', C Voiced AlveoloPalatal Fricative)
  , ("x",  'x', C Unvoiced Uvular Fricative)
  , ("h",  'h', C Unvoiced Glottal Fricative)
  , ("l,", 'ļ', C Unvoiced Lateral Fricative)
  , ("c",  'c', C Unvoiced ApicoAlveolar Affricative)
  , ("z.", 'ẓ', C Voiced ApicoAlveolar Affricative)
  , ("c/", 'č', C Unvoiced AlveoloPalatal Affricative)
  , ("j",  'j', C Voiced AlveoloPalatal Affricative)
  , ("m",  'm', C Voiced Labial Nasal)
  , ("n",  'n', C Voiced ApicoDental Nasal)
  , ("n/", 'ň', C Voiced Velar Nasal)
  , ("r",  'r', C Voiced AlveolarRetroflex Tap)
  , ("l",  'l', C Voiced Lateral Liquid)
  , ("w",  'w', C Voiced LabioVelar Approximant)
  , ("y",  'y', C Voiced Palatal Approximant)
  , ("r/", 'ř', C Voiced Uvular Approximant)
  -- Vowels
  , ("i",  'i', V Unrounded Front High)
  , ("u;", 'ü', V Rounded Front High)
  , ("u",  'u', V Rounded Back High)
  , ("e",  'e', V Unrounded Front Mid)
  , ("o;", 'ö', V Rounded Front Mid)
  , ("e;", 'ë', V Unrounded Back Mid)
  , ("o",  'o', V Rounded Back Mid)
  , ("a",  'a', V Unrounded Central Low)
  , ("a;", 'ä', V Unrounded Back Low)
  ]

consonants :: [Phoneme]
consonants = filter isConsonant phonemes
  where isConsonant p = case p of (C _ _ _) -> True; _ -> False

vowels :: [Phoneme]
vowels = filter isVowel phonemes
  where isVowel p = case p of (V _ _ _) -> True; _ -> False

asciiReps :: [String]
asciiReps = map (\(a, _, _) -> a) phonemeChart 

unicodeChars :: [Char]
unicodeChars = map (\(_, u, _) -> u) phonemeChart

phonemes :: [Phoneme]
phonemes = map (\(_, _, p) -> p) phonemeChart

asciiToPhoneme :: String -> Maybe Phoneme
asciiToPhoneme a = lookup a (zip asciiReps phonemes)

unicodeToPhoneme :: Char -> Maybe Phoneme
unicodeToPhoneme a = lookup a (zip unicodeChars phonemes)

phonemeToAscii :: Phoneme -> Maybe String
phonemeToAscii p = lookup p (zip phonemes asciiReps)

phonemeToUnicode :: Phoneme -> Maybe Char
phonemeToUnicode p = lookup p (zip phonemes unicodeChars)

asciiToUnicode :: String -> Maybe Char
asciiToUnicode a = lookup a (zip asciiReps unicodeChars)

unicodeToAscii :: Char -> Maybe String
unicodeToAscii a = lookup a (zip unicodeChars asciiReps)

convert :: String -> Maybe String
convert = sequence . (map asciiToUnicode) . tokenize
  where 
    tokenize (a:b:xs) =
      if [a, b] `elem` asciiReps
      then [a, b]:tokenize xs
      else [a]:tokenize (b:xs)
    tokenize [a] = [[a]]
    tokenize [] = []

deconvert :: String -> Maybe String
deconvert = fmap concat . sequence . map unicodeToAscii
