module Mamkait.Phonology where

import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (groupBy)
import qualified Data.Bimap as BM
import qualified Data.Text as T
import Data.Text.ICU


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


grave, acute, circumflex, diaeresis, caron, underDot, cedilla :: T.Text
-- Combining accents
grave = "\x300"
acute = "\x301"
circumflex = "\x302"
diaeresis = "\x308"
caron = "\x30C"
underDot = "\x323"
cedilla = "\x327"

phonemeTable :: [(Phoneme, Char, T.Text)]
-- 1. Phoneme classification
-- 2. ASCII input character
-- 3. Unicode representation
phonemeTable = [
  -- Consonant
    (C Unvoiced Labial Stop,                'p', "p")
  , (C Voiced Labial Stop,                  'b', "b")
  , (C Unvoiced ApicoDental Stop,           't', "t")
  , (C Voiced ApicoDental Stop,             'd', "d")
  , (C Unvoiced Velar Stop,                 'k', "k")
  , (C Voiced Velar Stop,                   'g', "g")
  , (C Unvoiced Glottal Stop,               '\'', "\'")
  , (C Unvoiced LabioDental Fricative,      'f', "f")
  , (C Voiced LabioDental Fricative,        'v', "v")
  , (C Unvoiced InterDental Fricative,      'T', "t" <> cedilla)
  , (C Voiced InterDental Fricative,        'D', "d" <> cedilla)
  , (C Unvoiced ApicoAlveolar Fricative,    's', "s")
  , (C Voiced ApicoAlveolar Fricative,      'z', "z")
  , (C Unvoiced AlveoloPalatal Fricative,   'S', "s" <> caron)
  , (C Voiced AlveoloPalatal Fricative,     'Z', "z" <> caron)
  , (C Unvoiced Palatal Fricative,          'q', "c" <> cedilla)
  , (C Unvoiced Uvular Fricative,           'x', "x")
  , (C Unvoiced Glottal Fricative,          'h', "h")
  , (C Unvoiced Lateral Fricative,          'L', "l" <> cedilla)
  , (C Unvoiced ApicoAlveolar Affricative,  'c', "c")
  , (C Voiced ApicoAlveolar Affricative,    'j', "z" <> underDot)
  , (C Unvoiced AlveoloPalatal Affricative, 'C', "c" <> caron)
  , (C Voiced AlveoloPalatal Affricative,   'J', "j")
  , (C Voiced Labial Nasal,                 'm', "m")
  , (C Voiced ApicoDental Nasal,            'n', "n")
  , (C Voiced Velar Nasal,                  'N', "n" <> caron)
  , (C Voiced AlveolarRetroflex Tap,        'r', "r")
  , (C Voiced Lateral Liquid,               'l', "l")
  , (C Voiced LabioVelar Approximant,       'w', "w")
  , (C Voiced Palatal Approximant,          'y', "y")
  , (C Voiced Uvular Approximant,           'R', "r" <> caron)
  -- Vowels
  , (V Unrounded Front High,                'i', "i")
  , (V Unrounded Central High,              'I', "i" <> diaeresis)
  , (V Rounded Front High,                  'U', "u" <> diaeresis)
  , (V Rounded Back High,                   'u', "u")
  , (V Unrounded Front Mid,                 'e', "e")
  , (V Rounded Front Mid,                   'O', "o" <> diaeresis)
  , (V Unrounded Back Mid,                  'E', "e" <> diaeresis)
  , (V Rounded Back Mid,                    'o', "o")
  , (V Unrounded Central Low,               'a', "a")
  , (V Unrounded Back Low,                  'A', "a" <> diaeresis)
  ]

allPhonemes :: [Phoneme]
allPhonemes = map sel1 phonemeTable

asciiCodes :: [Char]
asciiCodes = map sel2 phonemeTable

unicodeReps :: [T.Text]
unicodeReps = map sel3 phonemeTable

asciiMap :: BM.Bimap Phoneme Char
asciiMap = BM.fromList $ zip allPhonemes asciiCodes

unicodeMap :: BM.Bimap Phoneme T.Text
unicodeMap = BM.fromList $ zip allPhonemes unicodeReps

isConsonant, isVowel :: Phoneme -> Bool
isConsonant C {} = True
isConsonant V {} = False
isVowel C {} = False
isVowel V {} = True

consonants, vowels :: [Phoneme]
consonants = filter isConsonant allPhonemes
vowels = filter isVowel allPhonemes

vowelReps, consonantReps :: [T.Text]
vowelReps = map phonemeToUnicode vowels
consonantReps = map phonemeToUnicode consonants

phonemeFromAscii :: Char -> Maybe Phoneme
phonemeFromAscii c = BM.lookupR c asciiMap

fromAscii :: T.Text -> PString
fromAscii = mapMaybe phonemeFromAscii . T.unpack

phonemeToAscii :: Phoneme -> T.Text
phonemeToAscii p = T.singleton $ fromJust $ BM.lookup p asciiMap

toAscii :: PString -> T.Text
toAscii ps = T.concat $ map phonemeToAscii ps

phonemeFromUnicode :: T.Text -> Maybe Phoneme
phonemeFromUnicode c = BM.lookupR c unicodeMap

fromUnicode :: T.Text -> PString
fromUnicode = mapMaybe phonemeFromUnicode . breakCharacters . normalize NFD

breakCharacters :: T.Text -> [T.Text]
breakCharacters = map brkBreak . breaks (breakCharacter Root)

phonemeToUnicode :: Phoneme -> T.Text
phonemeToUnicode p = fromJust $ BM.lookup p unicodeMap

toUnicode :: PString -> T.Text
toUnicode ps = T.concat $ map phonemeToUnicode ps


-- Conjuncts and stress markers

data Conjunct
  = CConj PString -- consonantal conjunct
  | VConj Bool PString -- stress, vocalic conjunct
  deriving (Show, Eq, Ord)

stressMarker :: Char
stressMarker = ';'

stressVowel :: T.Text -> T.Text
stressVowel s
  | len == 1  = vowel <> acute
  | len == 2  =
    if accent == diaeresis
      then vowel <> circumflex
    else s
  | otherwise  = s
  where
    len = T.length s
    vowel = T.take 1 s
    accent = T.takeEnd 1 s

unstressVowel :: T.Text -> T.Text
unstressVowel s
  | len == 1  = s
  | len == 2  =
    if accent == acute
      then vowel
    else if accent == circumflex
      then vowel <> diaeresis
    else s
  | otherwise  = s
  where
    len = T.length s
    vowel = T.take 1 s
    accent = T.takeEnd 1 s

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
getStresses = mapMaybe getStress

addStress :: Conjunct -> Conjunct
addStress (VConj _ ps) = VConj True ps
addStress conj = conj

removeStress :: Conjunct -> Conjunct
removeStress (VConj _ ps) = VConj False ps
removeStress conj = conj

conjunctsFromAscii :: T.Text -> [Conjunct]
conjunctsFromAscii s = addDefaultStress $ map conjunctFromAscii $ splitConjunctsAscii s

conjunctsFromUnicode :: T.Text -> [Conjunct]
conjunctsFromUnicode s = addDefaultStress $ map conjunctFromUnicode $ splitConjunctsUnicode s

splitConjunctsAscii :: T.Text -> [T.Text]
splitConjunctsAscii = T.groupBy sameType
  where
    sameType :: Char -> Char -> Bool
    sameType a b = isVowel' a == isVowel' b
    isVowel' :: Char -> Bool
    isVowel' c
      | c == stressMarker  = True -- Count the stress marker as a vowel for splitting purposes.
      | otherwise  = maybe True isVowel $ phonemeFromAscii c

splitConjunctsUnicode :: T.Text -> [T.Text]
splitConjunctsUnicode s = map T.concat $ groupBy sameType $ breakCharacters s
  where
    sameType :: T.Text -> T.Text -> Bool
    sameType a b = isVowel'' a == isVowel'' b
    isVowel'' :: T.Text -> Bool
    isVowel'' c = maybe True isVowel $ phonemeFromUnicode $ unstressVowel c

conjunctFromAscii :: T.Text -> Conjunct
conjunctFromAscii s
  | startsWithConsonant  = CConj ps
  | endsWithStressMarker  = VConj True $ fromAscii $ T.init s
  | otherwise  = VConj False ps
  where
    ps = fromAscii s
    startsWithConsonant = not (null ps) && isConsonant (head ps)
    endsWithStressMarker = not (T.null s) && T.last s == stressMarker

conjunctFromUnicode :: T.Text -> Conjunct
conjunctFromUnicode s
  | startsWithConsonant  = CConj ps
  | hasStress  = VConj True $ fromUnicode unstressed
  | otherwise  = VConj False ps
  where
    ps = fromUnicode s
    startsWithConsonant = not (null ps) && isConsonant (head ps)
    s' = normalize NFD s
    unstressed = T.concat $ map unstressVowel $ breakCharacters s'
    hasStress = s' /= unstressed

vowelIndex :: Int -> [Conjunct] -> Maybe Int
-- vowel index 0 is the ultimate syllable
-- vowel index 1 is the penultimate syllable
-- vowel index 2 is the antepenultimate syllable
vowelIndex n conjs
  | ix >= 0  = Just (vowelIndices !! ix)
  | otherwise  = Nothing
  where
    vowelIndices = [ i | (i, conj) <- zip [0..] conjs, isVowelConjunct conj ]
    ix = length vowelIndices - 1 - n

addDefaultStress :: [Conjunct] -> [Conjunct]
-- If the formative is completely unstressed, add default penultimate stress.
addDefaultStress conjs =
  case vowelIndex 1 conjs of
    Nothing -> conjs
    Just i ->
      if all (==False) (getStresses conjs)
        then modifyNth i addStress conjs
        else conjs

removeDefaultStress :: [Conjunct] -> [Conjunct]
-- Never display penultimate stress.
removeDefaultStress conjs =
  case vowelIndex 1 conjs of
    Nothing -> conjs
    Just i -> modifyNth i removeStress conjs

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
  | n == 0  = f x : xs
  | otherwise  = x : modifyNth (n-1) f xs

conjunctToAscii :: Conjunct -> T.Text
conjunctToAscii conj =
  case getStress conj of
    Just True -> s `T.snoc` stressMarker
    _ -> s
  where s = toAscii $ getString conj

conjunctToUnicode :: Conjunct -> T.Text
conjunctToUnicode conj =
  case getStress conj of
    Just True -> T.concat $ modifyNth 0 stressVowel $ breakCharacters s
    _ -> s
  where s = toUnicode $ getString conj

conjunctsToAscii :: [Conjunct] -> T.Text
conjunctsToAscii = T.concat . map conjunctToAscii . removeDefaultStress

conjunctsToUnicode :: [Conjunct] -> T.Text
conjunctsToUnicode = T.concat . map conjunctToUnicode . removeDefaultStress

conjunctsToUnicodeHyphenated :: [Conjunct] -> T.Text
conjunctsToUnicodeHyphenated = T.intercalate "-" . map conjunctToUnicode . removeDefaultStress

lexSentence :: T.Text -> [[Conjunct]]
lexSentence s =
  -- Determine whether to parse the sentence as ASCII or Unicode.
  let
    asciiAlphabet = stressMarker : asciiCodes
    asciiCount = T.length $ T.filter (`elem` asciiAlphabet) s
    stressedVowels = map (stressVowel . phonemeToUnicode) vowels
    unicodeAlphabet = unicodeReps ++ stressedVowels
    unicodeCount = length $ filter (`elem` unicodeAlphabet) $ breakCharacters s
  in
    if asciiCount > unicodeCount
      then map conjunctsFromAscii $ T.words s
      else map conjunctsFromUnicode $ T.words s


-- Vowel Forms

vowelFormTable :: [[T.Text]]
vowelFormTable =
  [ ["a",  "A",  "e",  "I",  "i",  "O",  "o",  "U",  "u" ]
  , ["ai", "au", "ei", "eu", "Ei", "ou", "oi", "iu", "ui"]
  , ["ia", "iA", "ie", "iE", "Eu", "uO", "uo", "ue", "ua"]
  , ["ao", "ae", "ea", "eo", "eE", "Oe", "oe", "Oa", "oa"]
  ]

vowelForm :: Int -> Int -> Conjunct
vowelForm series form = conjunctFromAscii $ vowelFormTable !! (series-1) !! (form-1)

altY :: T.Text -> T.Text
-- Alternate vowel forms to prevent y-i
altY "ia" = "oA"
altY "iA" = "uA"
altY "ie" = "oE"
altY "iE" = "uE"
altY s = s

altW :: T.Text -> T.Text
-- Alternate vowel forms to prevent w-u
altW "uO" = "iO"
altW "uo" = "io"
altW "ue" = "eO"
altW "ua" = "aO"
altW s = s
