module Quijada.Cr
  ( Cr
  , cr
  , renderCr
  ) where

import Quijada.Phonology
  ( PString
  , pstring
  , isVowel
  , renderP
  , render
  )
import Quijada.Error

-- Section 3.3, Slot III: Cr - The Main Root 

data Cr = Cr PString
  deriving (Show, Eq)

cr :: String -> Either [Error] Cr
cr s = either Left check $ pstring s
  where
    check ps
      | length ps < 1 || length ps > 6  = Left [ WrongSize $ length ps ]
      | any isVowel ps  = Left [ IllegalVowel (renderP p) | p <- ps, isVowel p ]
      -- TODO: phonotaxis
      | otherwise  = Right (Cr ps)

renderCr :: Cr -> String
renderCr (Cr ps) = render ps
