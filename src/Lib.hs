module Lib where

import qualified Data.List  as L
import qualified Data.Maybe as M
import           Defaults
import           Types

-- | Renders a given text as ascii art
render :: String   -- ^ String to render
       -> Font     -- ^ Font to use
       -> String   -- ^ Rendered text
render txt font =
  L.intercalate "\n" $
  map (map pointToChar . concat)
      (L.transpose (map findChar txt))
  where
    findChar c = M.fromMaybe (error $ "No data for char: " ++ [c])
      (lookup c (mapping font))
    pointToChar p =
      case p of
        W -> ' '
        B -> filler font
