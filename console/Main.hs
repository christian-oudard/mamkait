module Main where

import System.IO
import Data.List (intersperse)
import Mamkait.Phonology
  ( chars
  , asciiCodes
  , lexConjuncts
  , render
  , renderHyphenated
  )
-- import Mamkait.Error

main :: IO ()
main = do
  let title = " Mamkait "
  putStrLn $ take (length title) $ repeat '-'
  putStrLn title
  putStrLn $ take (length title) $ repeat '-'
  putStrLn ""
  putStrLn $ intersperse ' ' chars
  putStrLn $ intersperse ' ' asciiCodes
  putStrLn ""
  loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  command <- getLine
  putStrLn $ unwords $ map (render . lexConjuncts) $ words command
  putStrLn $ unlines $ map (renderHyphenated . lexConjuncts) $ words command
  if command /= "exit"
  then loop
  else return ()
