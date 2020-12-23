module Main where

import System.IO
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)

import Mamkait.Phonology
  ( reps
  , asciiCodes
  , lexConjuncts
  , render
  , renderHyphenated
  )
-- import Mamkait.Error

main :: IO ()
main = do
  let title = " Mamkait "
  putStrLn $ replicate (length title) '-'
  putStrLn title
  putStrLn $ replicate (length title) '-'
  putStrLn ""
  TIO.putStrLn $ T.intercalate " " reps
  putStrLn $ intersperse ' ' asciiCodes
  putStrLn ""
  loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  command <- TIO.getLine
  TIO.putStrLn $ T.unwords $ map (render . lexConjuncts) $ T.words command
  TIO.putStrLn $ T.unlines $ map (renderHyphenated . lexConjuncts) $ T.words command
  when (command /= "exit") loop
