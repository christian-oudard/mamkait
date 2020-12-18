module Main where

import System.IO
import Data.List (intersperse, intercalate)
import Mamkait.Phonology
  ( pstring
  , render
  , chars
  , asciiCodes
  )
import Mamkait.Error

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
  putStrLn $ convert command
  putStrLn ""
  if command /= "exit"
  then loop
  else return ()

convert :: String -> String
convert s = 
  case sequenceLefts $ map pstring $ words s of
  Left errors -> unlines $ map show $ concat errors
  Right results -> unwords $ map render results
