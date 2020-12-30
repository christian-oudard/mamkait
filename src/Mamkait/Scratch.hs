module Mamkait.Scratch where

import Data.Tuple
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Replace
import qualified Data.Text.IO as TIO
import qualified Data.Bimap as BM
import Control.Monad (when)

import Mamkait.Error
import Mamkait.Phonology
import Mamkait.Grammar
import Mamkait.Phonotaxis


allOf :: (Enum a, Bounded a) => [a]
allOf = [minBound .. maxBound]

ceape :: [(Configuration, Extension, Affiliation, Perspective, Essence)]
ceape = (,,,,) <$> allOf <*> allOf <*> allOf <*> allOf <*> allOf

checkCa :: IO ()
checkCa = mapM_ out ceape
  where
    out (co, ex, af, pe, es) = do
      -- if impermissible $ fromAscii ca' then
      TIO.putStrLn $ T.pack (show (co, ex, af, pe, es)) <> " " <> u ca <> " " <> u ca' <> " " <> star
        -- else return ()
      where
        u = toUnicode . fromAscii
        ca = constructCa (co, ex, af, pe, es)
        ca' = substituteAllomorphic forwardSubs $ ca
        star = if ca /= ca' then "*" else ""
