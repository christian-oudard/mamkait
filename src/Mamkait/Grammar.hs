module Mamkait.Grammar where

import Data.Maybe (fromJust)
import qualified Data.Bimap as BM
import Mamkait.Phonology (Conjunct, vowelForm)


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
