module Mamkait.Grammar where

import Data.Tuple (swap)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.ICU (Regex, regex)
import Data.Text.ICU.Replace (Replace, replace, rtext)
import qualified Data.Bimap as BM
import Mamkait.Phonology
  ( Conjunct
  , conjunctFromAscii
  , asciiCodes
  , vowelForm
  )


-- Slot IV: V_R - Function, Specification, Context

type SlotIV = (Function, Specification, Context)

data Function
  = STA -- Stative
  | DYN -- Dynamic
  deriving (Show, Eq, Ord)

data Specification
  = BSC -- Basic
  | CTE -- Contential
  | CSV -- Constitutive
  | OBJ -- Objective
  deriving (Show, Eq, Ord)

data Context
  = EXS -- Existential
  | FNC -- Functional
  | RPS -- Representational
  | AMG -- Amalgamative
  deriving (Show, Eq, Ord)

vrTable :: BM.Bimap SlotIV Conjunct
vrTable = BM.fromList
  [ ((STA, BSC, EXS), vowelForm 1 1)
  , ((STA, CTE, EXS), vowelForm 1 2)
  , ((STA, CSV, EXS), vowelForm 1 3)
  , ((STA, OBJ, EXS), vowelForm 1 5) -- Form 5, "i" not "I".
  , ((DYN, BSC, EXS), vowelForm 1 6)
  , ((DYN, CTE, EXS), vowelForm 1 7)
  , ((DYN, CSV, EXS), vowelForm 1 8)
  , ((DYN, OBJ, EXS), vowelForm 1 9)
  , ((STA, BSC, FNC), vowelForm 2 1)
  , ((STA, CTE, FNC), vowelForm 2 2)
  , ((STA, CSV, FNC), vowelForm 2 3)
  , ((STA, OBJ, FNC), vowelForm 2 4)
  , ((DYN, BSC, FNC), vowelForm 2 6)
  , ((DYN, CTE, FNC), vowelForm 2 7)
  , ((DYN, CSV, FNC), vowelForm 2 8)
  , ((DYN, OBJ, FNC), vowelForm 2 9)
  , ((STA, BSC, RPS), vowelForm 3 1)
  , ((STA, CTE, RPS), vowelForm 3 2)
  , ((STA, CSV, RPS), vowelForm 3 3)
  , ((STA, OBJ, RPS), vowelForm 3 4)
  , ((DYN, BSC, RPS), vowelForm 3 6)
  , ((DYN, CTE, RPS), vowelForm 3 7)
  , ((DYN, CSV, RPS), vowelForm 3 8)
  , ((DYN, OBJ, RPS), vowelForm 3 9)
  , ((STA, BSC, AMG), vowelForm 4 1)
  , ((STA, CTE, AMG), vowelForm 4 2)
  , ((STA, CSV, AMG), vowelForm 4 3)
  , ((STA, OBJ, AMG), vowelForm 4 4)
  , ((DYN, BSC, AMG), vowelForm 4 6)
  , ((DYN, CTE, AMG), vowelForm 4 7)
  , ((DYN, CSV, AMG), vowelForm 4 8)
  , ((DYN, OBJ, AMG), vowelForm 4 9)
  ]

slotIVToVr :: SlotIV -> Conjunct
slotIVToVr slot = fromJust $ BM.lookup slot vrTable

vrToSlotIV :: Conjunct -> Maybe SlotIV
vrToSlotIV conj = BM.lookupR conj vrTable


-- Slot VI: C_A - Configuration, Extension, Affiliation, Perspective, Essence

type SlotVI = (Configuration, Extension, Affiliation, Perspective, Essence)

data Configuration
  = UNI -- Uniplex
  | DSS -- Duplex Similar Separate
  | DSC -- Duplex Similar Connected
  | DSF -- Duplex Similar Fused
  | DDS -- Duplex Dissimilar Separate
  | DDC -- Duplex Dissimilar Connected
  | DDF -- Duplex Dissimilar fused
  | DFS -- Duplex Fuzzy Separate
  | DFC -- Duplex Fuzzy Connected
  | DFF -- Duplex Fuzzy Fused
  | MSS -- Multiplex Similar Separate
  | MSC -- Multiplex Similar Connected
  | MSF -- Multiplex Similar Fused
  | MDS -- Multiplex Dissimilar Separate
  | MDC -- Multiplex Dissimilar Connected
  | MDF -- Multiplex Dissimilar Fused
  | MFS -- Multiplex Fuzzy Separate
  | MFC -- Multiplex Fuzzy Connected
  | MFF -- Multiplex Fuzzy Fused
  deriving (Show, Eq, Ord, Enum, Bounded)

data Extension
  = DEL -- Delimitive
  | PRX -- Proximal
  | ICP -- Incipient
  | ATV -- Attenuative
  | GRA -- Graduative
  | DPL -- Depletive
  deriving (Show, Eq, Ord, Enum, Bounded)

data Affiliation
  = CSL -- Consolidative
  | ASO -- Associative
  | COA -- Coalescent
  | VAR -- Variative
  deriving (Show, Eq, Ord, Enum, Bounded)

data Perspective
  = M -- Monadic
  | P -- Polyadic
  | N -- Nomic
  | A -- Abstract
  deriving (Show, Eq, Ord, Enum, Bounded)

data Essence
  = NRM -- Normal
  | RPV -- Representative
  deriving (Show, Eq, Ord, Enum, Bounded)


ca1Table :: BM.Bimap Configuration T.Text
ca1Table = BM.fromList
  [ (UNI, "")
  , (DSS, "rt")
  , (DSC, "rk")
  , (DSF, "rp")
  , (DDS, "rn")
  , (DDC, "rN")
  , (DDF, "rm")
  , (DFS, "Rt")
  , (DFC, "Rk")
  , (DFF, "Rp")
  , (MSS, "t")
  , (MSC, "k")
  , (MSF, "p")
  , (MDS, "n")
  , (MDC, "N")
  , (MDF, "m")
  , (MFS, "lt")
  , (MFC, "lk")
  , (MFF, "lp")
  ]

ca2Table :: BM.Bimap Extension T.Text
ca2Table = BM.fromList
  [ (DEL, "")
  , (PRX, "s")
  , (ICP, "S")
  , (ATV, "f")
  , (GRA, "T")
  , (DPL, "q")
  ]

ca3Table :: BM.Bimap Affiliation (T.Text, T.Text)
ca3Table = BM.fromList
  [ (CSL, ("", ""))
  , (ASO, ("d", "t"))
  , (COA, ("g", "k"))
  , (VAR, ("b", "p"))
  ]

ca4Table :: BM.Bimap (Perspective, Essence) (T.Text, T.Text)
ca4Table = BM.fromList
  [ ((M, NRM), ("l", ""))
  , ((P, NRM), ("r", "r"))
  , ((N, NRM), ("v", "w"))
  , ((A, NRM), ("z", "y"))
  , ((M, RPV), ("R", "R"))
  , ((P, RPV), ("tL", "l"))
  , ((N, RPV), ("lm", "m"))
  , ((A, RPV), ("ln", "n"))
  ]

constructCa :: SlotVI -> T.Text
constructCa (co, ex, af, pe, es) = ca1 <> ca2 <> ca3' <> ca4'
  where
    ca1 = fromJust $ BM.lookup co ca1Table
    ca2 = fromJust $ BM.lookup ex ca2Table
    ca3 = fromJust $ BM.lookup af ca3Table
    ca3' = if co == UNI && ex == DEL then fst ca3 else snd ca3
    ca4 = fromJust $ BM.lookup (pe, es) ca4Table
    ca4' = if co == UNI && ex == DEL && af == CSL then fst ca4 else snd ca4

substituteAllomorphic :: [(Regex, Replace)] -> T.Text -> T.Text
substituteAllomorphic subs str = foldl (\s (a, b) -> replace a b s) str subs

forwardSubs, reverseSubs :: [(Regex, Replace)]
forwardSubs = makeReplacements substitutions
reverseSubs = makeReplacements $ map swap $ reverse substitutions

makeReplacements :: [(T.Text, T.Text)] -> [(Regex, Replace)]
makeReplacements = map construct
  where
    construct (a, b) = (regex [] a, rtext b')
      where b' = T.filter (`elem` asciiCodes) b -- Don't put regex syntax in replacements.

substitutions :: [(T.Text, T.Text)]
substitutions =
  [ ("ts", "c")
  , ("tS", "C")
  , ("tT", "D")
  , ("np", "mv")
  , ("Nk", "Nz")
  , ("nf(?=.)", "v(?=.)")
  , ("tf", "fs")
  , ("kf", "fS")
  , ("Ny", "NZ")
  , ("qy", "Z")
  , ("cy", "j")
  , ("Cy", "J")
  , ("^tt", "^nd")
  , ("^kk", "^ng")
  , ("^pp", "^mb")
  , ("nn", "nz")
  , ("mm", "mz")
  , ("ltt", "ld")
  , ("lkk", "lg")
  , ("lpp", "lb")
  , ("rnm", "nZ")
  , ("rmn", "mZ")
  , ("rtt", "rd")
  , ("rkk", "rg")
  , ("rpp", "rb")
  , ("rNm", "Nv")
  , ("rNn", "nD")
  , ("Rtt", "Rd")
  , ("Rkk", "Rg")
  , ("Rpp", "Rb")
  -- , ("Rtr", "Rtv")
  -- , ("Rkr", "Rkv")
  -- , ("Rpr", "Rpv")
  -- , ("Rtr", "Rtv")
  -- , ("Rkr", "Rkv")
  -- , ("Rpr", "Rpv")
  -- , ("Rtsr", "Rcv")
  -- , ("RNkr", "RNzv")
  ]


-- Bias Adjuncts

data Bias
  = DOL -- Dolorous
  | SKP -- Skeptical
  | IPT -- Impatient
  | RVL -- Revelative
  | TRP -- Trepidative
  | RPU -- Repulsive
  | PSC -- Prosaic
  | CMD -- Comedic
  | PPV -- Propositive
  | SGS -- Suggestive
  | DFD -- Diffident
  | RFL -- Reflective
  | DES -- Desperative
  | DPB -- Disapprobative
  | CTP -- Contemptive
  | EXA -- Exasperative
  | IDG -- Indignative
  | DIS -- Dismissive
  | DRS -- Derisive
  | PES -- Pessimistic
  | DUB -- Dubitative
  | IVD -- Invidious
  | DCC -- Disconcertive
  | STU -- Stupefactive
  | FSC -- Fascinative
  | IFT -- Infatuative
  | EUH -- Euphoric
  | EUP -- Euphemistic
  | CRR -- Corrective
  | APB -- Approbative
  | IRO -- Ironic
  | PSM -- Presumptive
  | GRT -- Gratificative
  | SAT -- Satiative
  | PPX -- Perplexive
  | CTV -- Contemplative
  | PPT -- Propitious
  | SOL -- Solicitative
  | RAC -- Reactive
  | COI -- Coincidental
  | FOR -- Fortuitous
  | ANN -- Annunciative
  | DLC -- Delectative
  | ATE -- Attentive
  | RNC -- Renunciative
  | MAN -- Mandatory
  | EXG -- Exigent
  | ISP -- Insipid
  | ADM -- Admissive
  | APH -- Apprehensive
  | OPT -- Optimal
  | CNV -- Contensive
  | IPL -- Implicative
  | ACC -- Accidental
  | ANP -- Anticipative
  | ACH -- Archetypal
  | VEX -- Vexative
  | CRP -- Corruptive
  | DEJ -- Dejective
  deriving (Show, Eq, Ord)

biasTable :: BM.Bimap Conjunct Bias
biasTable = BM.map conjunctFromAscii $ BM.fromList
  [ ("RRx", DOL)
  , ("rnZ", SKP)
  , ("ZZv", IPT)
  , ("mmL", RVL)
  , ("llC", TRP)
  , ("SStL", RPU)
  , ("ZZt", PSC)
  , ("pLL", CMD)
  , ("sl", PPV)
  , ("ltq", SGS)
  , ("cC", DFD)
  , ("llm", RFL)
  , ("mRR", DES)
  , ("ffx", DPB)
  , ("kSS", CTP)
  , ("kqq", EXA)
  , ("pSS", IDG)
  , ("kff", DIS)
  , ("pfc", DRS)
  , ("ksp", PES)
  , ("mmf", DUB)
  , ("RRn", IVD)
  , ("gzJ", DCC)
  , ("LLC", STU)
  , ("ZZJ", FSC)
  , ("vvr", IFT)
  , ("gzz", EUH)
  , ("vvt", EUP)
  , ("NT", CRR)
  , ("Rs", APB)
  , ("mmZ", IRO)
  , ("nnT", PSM)
  , ("mmh", GRT)
  , ("LT", SAT)
  , ("llh", PPX)
  , ("gvv", CTV)
  , ("mll", PPT)
  , ("NNs", SOL)
  , ("kll", RAC)
  , ("SSC", COI)
  , ("lzp", FOR)
  , ("drr", ANN)
  , ("jmm", DLC)
  , ("NJ", ATE)
  , ("mzt", RNC)
  , ("msk", MAN)
  , ("rrs", EXG)
  , ("lqp", ISP)
  , ("lL", ADM)
  , ("vvz", APH)
  , ("CCk", OPT)
  , ("rrJ", CNV)
  , ("vll", IPL)
  , ("lf", ACC)
  , ("lst", ANP)
  , ("mqt", ACH)
  , ("ksk", VEX)
  , ("gZZ", CRP)
  , ("ZZg", DEJ)
  ]

adjunctToBias :: Conjunct -> Maybe Bias
adjunctToBias conj = BM.lookup conj biasTable

biasToAdjunct :: Bias -> Conjunct
biasToAdjunct slot = fromJust $ BM.lookupR slot biasTable
