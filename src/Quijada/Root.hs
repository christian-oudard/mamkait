module Quijada.Root where

import Quijada.Phonology
  ( PString
  , pstring
  , isVowel
  , renderP
  )
import Quijada.Error

data Root = Cr PString
  deriving (Show, Eq)

root :: String -> Either [Error] Root
root s = either Left check $ pstring s
  where
    check ps
      | length ps < 1 || length ps > 6  = Left [ WrongSize $ length ps ]
      | any isVowel ps  = Left [ IllegalVowel (renderP p) | p <- ps, isVowel p ]
      -- TODO: phonotaxis
      | otherwise  = Right (Cr ps)
