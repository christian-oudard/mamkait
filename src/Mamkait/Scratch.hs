module Mamkait.Scratch where

import Data.Tuple
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bimap as BM

import Mamkait.Error
import Mamkait.Phonology
import Mamkait.Grammar


allOf :: (Enum a, Bounded a) => [a]
allOf = [minBound .. maxBound]

cpe :: [(Configuration, Perspective, Essence)]
cpe = (,,) <$> allOf <*> allOf <*> allOf

checkCPE :: IO ()
checkCPE = mapM_ out cpe
  where
    out (c, p, e) = TIO.putStrLn $ T.pack (show (c,p,e)) <> " " <> constructCa c p e
