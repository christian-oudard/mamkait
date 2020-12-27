module Mamkait.Grammar where

import Data.Maybe (fromJust)
import qualified Data.Bimap as BM
import Mamkait.Phonology
  ( Conjunct
  , makeConj
  , vowelForm
  )


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

vrTable :: BM.Bimap Conjunct SlotIV
vrTable = BM.fromList
  [ (vowelForm 1 1, (STA, BSC, EXS))
  , (vowelForm 1 2, (STA, CTE, EXS))
  , (vowelForm 1 3, (STA, CSV, EXS))
  , (vowelForm 1 5, (STA, OBJ, EXS)) -- Form 5, "i" not "I".
  , (vowelForm 1 6, (DYN, BSC, EXS))
  , (vowelForm 1 7, (DYN, CTE, EXS))
  , (vowelForm 1 8, (DYN, CSV, EXS))
  , (vowelForm 1 9, (DYN, OBJ, EXS))
  , (vowelForm 2 1, (STA, BSC, FNC))
  , (vowelForm 2 2, (STA, CTE, FNC))
  , (vowelForm 2 3, (STA, CSV, FNC))
  , (vowelForm 2 4, (STA, OBJ, FNC))
  , (vowelForm 2 6, (DYN, BSC, FNC))
  , (vowelForm 2 7, (DYN, CTE, FNC))
  , (vowelForm 2 8, (DYN, CSV, FNC))
  , (vowelForm 2 9, (DYN, OBJ, FNC))
  , (vowelForm 3 1, (STA, BSC, RPS))
  , (vowelForm 3 2, (STA, CTE, RPS))
  , (vowelForm 3 3, (STA, CSV, RPS))
  , (vowelForm 3 4, (STA, OBJ, RPS))
  , (vowelForm 3 6, (DYN, BSC, RPS))
  , (vowelForm 3 7, (DYN, CTE, RPS))
  , (vowelForm 3 8, (DYN, CSV, RPS))
  , (vowelForm 3 9, (DYN, OBJ, RPS))
  , (vowelForm 4 1, (STA, BSC, AMG))
  , (vowelForm 4 2, (STA, CTE, AMG))
  , (vowelForm 4 3, (STA, CSV, AMG))
  , (vowelForm 4 4, (STA, OBJ, AMG))
  , (vowelForm 4 6, (DYN, BSC, AMG))
  , (vowelForm 4 7, (DYN, CTE, AMG))
  , (vowelForm 4 8, (DYN, CSV, AMG))
  , (vowelForm 4 9, (DYN, OBJ, AMG))
  ]

vrToSlotIV :: Conjunct -> Maybe SlotIV
vrToSlotIV conj = BM.lookup conj vrTable

slotIVToVr :: SlotIV -> Conjunct
slotIVToVr slot = fromJust $ BM.lookupR slot vrTable


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
biasTable = BM.map makeConj $ BM.fromList
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
