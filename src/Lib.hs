module Lib where

import Types
import Defaults
import qualified Data.List as L

-- | Renders a given text as ascii art
render :: String   -- ^ String to render
       -> Font     -- ^ Font to use
       -> String   -- ^ Rendered text
render txt font = 
  L.concat $
  L.intersperse "\n" $
  map (\l -> map pointToChar (concat l))
      (L.transpose (map findChar txt))
  where findChar c = 
          case lookup c (mapping font) of
            Nothing -> error $ "No data for char: " ++ [c]
            Just m -> m
        pointToChar p = 
          case p of
            W -> ' '
            B -> filler font
