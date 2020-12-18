module Quijada.Error
  ( Error(..)
  ) where

data Error
  = UnknownChar Char
  | IllegalVowel Char
  | IllegalConsonant Char
  | WrongSize Int
  deriving (Show)


