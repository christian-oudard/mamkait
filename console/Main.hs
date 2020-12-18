module Main where

import System.IO
import Data.List (intersperse)
import Mamkait.Phonology
  ( pstring
  , render
  , chars
  , asciiCodes
  )

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
  putStrLn $ parse command
  putStrLn ""
  if command /= "exit"
  then loop
  else return ()


parse :: String -> String
parse s = either show render $ pstring s
