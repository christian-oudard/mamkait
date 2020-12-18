module Mamkait.Error
  ( Error(..)
  , sequenceLefts
  ) where

import Data.Either (lefts, rights)

data Error
  = UnknownChar Char
  | IllegalVowel Char
  | IllegalConsonant Char
  | WrongSize Int
  deriving (Show)

sequenceLefts :: [Either a b] -> Either [a] [b]
sequenceLefts xs =
  case lefts xs of
    [] -> Right $ rights xs
    _ -> Left $ lefts xs
