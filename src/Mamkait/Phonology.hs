module Mamkait.Phonology
  ( Phoneme
  , phoneme
  , PString
  , pstring
  , Conjunct
  , allPhonemes
  , chars
  , asciiCodes
  , consonants
  , vowels
  , isConsonant
  , isVowel
  , renderPhoneme
  , renderPString
  , isConsonantConjunct
  , isVowelConjunct
  , lexConjuncts
  , splitConjuncts
  , getString
  , getStress
  , getStresses
  , render
  , renderHyphenated
  ) where

import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Maybe (catMaybes)
import Data.List (groupBy, intercalate)
import qualified Data.Bimap as BM

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
isConsonant (C _ _ _) = True
isConsonant (V _ _ _) = False
isVowel (C _ _ _) = False
isVowel (V _ _ _) = True

consonants, vowels :: [Phoneme]
consonants = filter isConsonant allPhonemes
vowels = filter isVowel allPhonemes

asciiMap :: BM.Bimap Phoneme Char
asciiMap = BM.fromList $ zip allPhonemes asciiCodes

phoneme :: Char -> Maybe Phoneme
phoneme c = BM.lookupR c asciiMap

pstring :: String -> PString
pstring s = catMaybes $ map phoneme s

unicodeMap :: BM.Bimap Phoneme Char
unicodeMap = BM.fromList $ zip allPhonemes chars

renderPhoneme :: Phoneme -> Char
renderPhoneme p = maybe (error "broken phoneme chart") id $ BM.lookup p unicodeMap

renderPString :: PString -> String
renderPString ps = map renderPhoneme ps


-- Conjuncts and stress markers

data Conjunct
  = CConj PString -- consonantal conjunct
  | VConj Bool PString -- stress, vocalic conjunct
  deriving (Show, Eq)


stressMarker :: Char
stressMarker = ';'

vowelStress :: Char -> Char
vowelStress 'i' = 'í'
vowelStress 'ï' = 'î'
vowelStress 'ü' = 'û'
vowelStress 'u' = 'ú'
vowelStress 'e' = 'é'
vowelStress 'ö' = 'ô'
vowelStress 'ë' = 'ê'
vowelStress 'o' = 'ó'
vowelStress 'a' = 'á'
vowelStress 'ä' = 'â'
vowelStress c = error $ "cannot add stress to '" ++ [c] ++ "'"


isConsonantConjunct, isVowelConjunct :: Conjunct -> Bool
isConsonantConjunct (CConj _) = True
isConsonantConjunct (VConj _ _) = False
isVowelConjunct (CConj _) = False
isVowelConjunct (VConj _ _) = True

getString :: Conjunct -> PString
getString (CConj ps) = ps
getString (VConj _ ps) = ps

getStress :: Conjunct -> Maybe Bool
getStress (CConj _) = Nothing
getStress (VConj stress _) = Just stress

getStresses :: [Conjunct] -> [Bool]
getStresses conjs = catMaybes $ map getStress $ conjs

addStress :: Conjunct -> Conjunct
removeStress :: Conjunct -> Conjunct
addStress (VConj _ ps) = VConj True ps
addStress conj = conj
removeStress (VConj _ ps) = VConj False ps
removeStress conj = conj

lexConjuncts :: String -> [Conjunct]
lexConjuncts s = addDefaultStress $ map makeConj $ splitConjuncts s

splitConjuncts :: String -> [String]
splitConjuncts = groupBy sameType
  where
    sameType :: Char -> Char -> Bool
    sameType a b = isVowel' a == isVowel' b
    isVowel' :: Char -> Bool
    isVowel' c
      | c == stressMarker  = True -- Count the stress marker as a vowel for splitting purposes.
      | otherwise  = maybe True isVowel $ phoneme c

makeConj :: String -> Conjunct
makeConj s =
  if isConsonant $ head $ pstring s
    then CConj $ pstring s
  else if last s == stressMarker
    then VConj True $ pstring $ init s
    else VConj False $ pstring s

vowelIndex :: Int -> [Conjunct] -> Int
-- vowel index 0 is the ultimate syllable
-- vowel index 1 is the penultimate syllable
-- vowel index 2 is the antepenultimate syllable
vowelIndex n conjs =
    let vowelIndices = [ i | (i, conj) <- zip [0..] conjs, isVowelConjunct conj ]
    in vowelIndices !! (length vowelIndices - 1 - n)

addDefaultStress :: [Conjunct] -> [Conjunct]
-- If the formative is completely unstressed, add default penultimate stress.
addDefaultStress conjs
  | (length conjs >= 2 && all (==False) (getStresses conjs))
    = modifyNth (vowelIndex 1 conjs) addStress conjs
  | otherwise  = conjs

removeDefaultStress :: [Conjunct] -> [Conjunct]
-- Never display penultimate stress.
removeDefaultStress conjs
  | length conjs >= 2
    = modifyNth (vowelIndex 1 conjs) removeStress conjs
  | otherwise  = conjs

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
  | n == 0  = (f x):xs
  | otherwise  = x:modifyNth (n-1) f xs

renderConjunct :: Conjunct -> String
renderConjunct (CConj ps) = renderPString ps
renderConjunct (VConj stressed ps)
  | stressed  = modifyNth 0 vowelStress $ renderPString ps
  | otherwise  = renderPString ps

render :: [Conjunct] -> String
render = concatMap renderConjunct . removeDefaultStress

renderHyphenated :: [Conjunct] -> String
renderHyphenated = intercalate "-" . map renderConjunct . removeDefaultStress
