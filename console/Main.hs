module Main where

import System.IO
import Data.List (intersperse)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bimap as BM

import Mamkait.Phonology
import Mamkait.Grammar


main :: IO ()
main = do
  let title = " Mamkait "
  putStrLn $ replicate (length title) '-'
  putStrLn title
  putStrLn $ replicate (length title) '-'
  putStrLn ""
  TIO.putStrLn $ T.intercalate " " unicodeReps
  putStrLn $ intersperse ' ' asciiCodes
  loop

loop :: IO ()
loop = do
  putStrLn ""
  putStr "> "
  hFlush stdout
  command <- TIO.getLine
  case command of 
    "vr" -> showVrTable
    _ -> lexFormatives command
  when (command /= "exit") loop

lexFormatives :: T.Text -> IO ()
lexFormatives command = do
  -- Show the formative deconstruction.
  let formatives = lexSentence command
  TIO.putStrLn $ T.unwords $ map conjunctsToUnicode formatives
  TIO.putStrLn $ T.unwords $ map conjunctsToUnicodeHyphenated formatives

showVrTable :: IO ()
showVrTable = do
  putStrLn ""
  mapM_ (TIO.putStrLn . showLine) $ BM.toList vrTable
  where
    showLine (slot, conj) = justifyLeftU 2 " " (conjunctToUnicode conj) <> " " <> T.pack (show slot)

justifyLeftU :: Int -> T.Text -> T.Text -> T.Text
justifyLeftU k pad t
    | len >= k  = t
    | otherwise = T.concat $ t : replicate (k-len) pad
  where len = length $ breakCharacters t
