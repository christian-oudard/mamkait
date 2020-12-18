module Mamkait.Phonology
  ( Phoneme
  , phoneme
  , PString
  , pstring
  , allPhonemes
  , chars
  , asciiCodes
  , consonants
  , vowels
  , renderP
  , render
  , isConsonant
  , isVowel
  ) where

import Data.Tuple.Select
import qualified Data.Bimap as BM
import Mamkait.Error

-- Section 1: Phonology

-- Phonemic inventory

type PString = [Phoneme]

data Phoneme
  = C CVoicedness CPlace CManner
  | V VRoundedness VPlace VHeight
  deriving (Show, Eq, Ord)

data CVoicedness = Voiced | Unvoiced
  deriving (Show, Eq, Ord)
data CPlace
  = Labial | LabioDental | LabioVelar | ApicoDental | InterDental | ApicoAlveolar
  | Alveolar | AlveolarRetroflex | AlveoloPalatal | Palatal | Velar | Uvular | Glottal | Lateral
  deriving (Show, Eq, Ord)
data CManner = Stop | Fricative | Affricative | Nasal | Tap | Liquid | Approximant
  deriving (Show, Eq, Ord)

data VRoundedness = Rounded | Unrounded
  deriving (Show, Eq, Ord)
data VPlace = Front | Central | Back
  deriving (Show, Eq, Ord)
data VHeight = High | Mid | Low
  deriving (Show, Eq, Ord)


phonemeChart :: [(Phoneme, Char, Char)]
-- 1. Phoneme classification
-- 2. Unicode character
-- 3. ASCII character

phonemeChart = [
  -- Consonant
    (C Unvoiced Labial Stop,                'p', 'p')
  , (C Voiced Labial Stop,                  'b', 'b')
  , (C Unvoiced ApicoDental Stop,           't', 't')
  , (C Voiced ApicoDental Stop,             'd', 'd')
  , (C Unvoiced Velar Stop,                 'k', 'k')
  , (C Voiced Velar Stop,                   'g', 'g')
  , (C Unvoiced Glottal Stop,               '\'', '\'')
  , (C Unvoiced LabioDental Fricative,      'f', 'f')
  , (C Voiced LabioDental Fricative,        'v', 'v')
  , (C Unvoiced InterDental Fricative,      'ţ', 'T')
  , (C Voiced InterDental Fricative,        'ḑ', 'D')
  , (C Unvoiced ApicoAlveolar Fricative,    's', 's')
  , (C Voiced ApicoAlveolar Fricative,      'z', 'z')
  , (C Unvoiced AlveoloPalatal Fricative,   'š', 'S')
  , (C Voiced AlveoloPalatal Fricative,     'ž', 'Z')
  , (C Unvoiced Palatal Fricative,          'ç', 'q') 
  , (C Unvoiced Uvular Fricative,           'x', 'x')
  , (C Unvoiced Glottal Fricative,          'h', 'h')
  , (C Unvoiced Lateral Fricative,          'ļ', 'L')
  , (C Unvoiced ApicoAlveolar Affricative,  'c', 'c')
  , (C Voiced ApicoAlveolar Affricative,    'ẓ', 'j')
  , (C Unvoiced AlveoloPalatal Affricative, 'č', 'C')
  , (C Voiced AlveoloPalatal Affricative,   'j', 'J')
  , (C Voiced Labial Nasal,                 'm', 'm')
  , (C Voiced ApicoDental Nasal,            'n', 'n')
  , (C Voiced Velar Nasal,                  'ň', 'N')
  , (C Voiced AlveolarRetroflex Tap,        'r', 'r')
  , (C Voiced Lateral Liquid,               'l', 'l')
  , (C Voiced LabioVelar Approximant,       'w', 'w')
  , (C Voiced Palatal Approximant,          'y', 'y')
  , (C Voiced Uvular Approximant,           'ř', 'R')
  -- Vowels
  , (V Unrounded Front High,                'i', 'i')
  , (V Unrounded Central High,              'ï', 'I')
  , (V Rounded Front High,                  'ü', 'U')
  , (V Rounded Back High,                   'u', 'u')
  , (V Unrounded Front Mid,                 'e', 'e')
  , (V Rounded Front Mid,                   'ö', 'O')
  , (V Unrounded Back Mid,                  'ë', 'E')
  , (V Rounded Back Mid,                    'o', 'o')
  , (V Unrounded Central Low,               'a', 'a')
  , (V Unrounded Back Low,                  'ä', 'A')
  ]

allPhonemes :: [Phoneme]
allPhonemes = map sel1 phonemeChart

chars, asciiCodes :: [Char]
chars = map sel2 phonemeChart
asciiCodes = map sel3 phonemeChart

isConsonant, isVowel :: Phoneme -> Bool
isConsonant p = case p of (C _ _ _) -> True; _ -> False
isVowel p = case p of (V _ _ _) -> True; _ -> False

consonants, vowels :: [Phoneme]
consonants = filter isConsonant allPhonemes
vowels = filter isVowel allPhonemes

asciiMap :: BM.Bimap Phoneme Char
asciiMap = BM.fromList $ zip allPhonemes asciiCodes

phoneme :: Char -> Either Error Phoneme
phoneme c =
  case BM.lookupR c asciiMap of
    Just p -> Right p
    Nothing -> Left $ UnknownChar c

pstring :: String -> Either [Error] PString
pstring s = sequenceLefts $ map phoneme s

unicodeMap :: BM.Bimap Phoneme Char
unicodeMap = BM.fromList $ zip allPhonemes chars

renderP :: Phoneme -> Char
renderP p = maybe (error "broken phoneme chart") id $ BM.lookup p unicodeMap

render :: PString -> String
render ps = map renderP ps


-- Syllables and stress markers

data Syllable
  = CSyl PString
  | VSyl PString Stress
data Stress = Stressed | Unstressed

stress :: Char -> Char
stress 'i' = 'í'
stress 'ï' = 'î'
stress 'ü' = 'û'
stress 'u' = 'ú'
stress 'e' = 'é'
stress 'ö' = 'ô'
stress 'ë' = 'ê'
stress 'o' = 'ó'
stress 'a' = 'á'
stress 'ä' = 'â'
stress c = c
