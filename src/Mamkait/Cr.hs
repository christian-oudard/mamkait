module Mamkait.Cr where
  -- ( Cr
  -- , cr
  -- , renderCr
  -- ) where

import Mamkait.Phonology (Conjunct)


-- Section 3.3, Slot III: Cr - The Main Root 

data Cr = Cr Conjunct
  deriving (Show, Eq)

-- cr s = either Left check $ pstring s
--   where
--     check ps
--       | length ps < 1 || length ps > 6  = Left [ WrongSize $ length ps ]
--       | any isVowel ps  = Left [ IllegalVowel (renderP p) | p <- ps, isVowel p ]
--       -- TODO: phonotaxis
--       | otherwise  = Right (Cr ps)
