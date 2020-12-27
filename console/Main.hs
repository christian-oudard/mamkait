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
  TIO.putStrLn $ T.intercalate " " reps
  putStrLn $ intersperse ' ' asciiCodes
  putStrLn ""
  loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  command <- TIO.getLine
  case command of 
    "vr" -> showVrTable
    _ -> lexFormative command
  when (command /= "exit") loop

lexFormative :: T.Text -> IO ()
lexFormative command = do
    TIO.putStrLn $ T.unwords $ map (render . lexConjuncts) $ T.words command
    TIO.putStrLn $ T.unlines $ map (renderHyphenated . lexConjuncts) $ T.words command

showVrTable :: IO ()
showVrTable = do
  putStrLn ""
  mapM_ (TIO.putStrLn . showLine) $ BM.toList vrTable
  where
    showLine (conj, slot) = justifyLeftU 2 " " (renderConjunct conj) <> " " <> T.pack (show slot)

justifyLeftU :: Int -> T.Text -> T.Text -> T.Text
justifyLeftU k pad t
    | len >= k  = t
    | otherwise = T.concat $ t : replicate (k-len) pad
  where len = length $ breakCharacters t
