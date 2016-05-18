module Lib where

import Types
import Defaults

import qualified Data.List.Split as S

-- | Renders a given text as ascii art
render :: String   -- ^ String to render
       -> Font     -- ^ Font to use       
       -> [String] -- ^ Lines of rendered text 
render txt font = 
  S.chunksOf (size font * length txt) $ concatSegments (map findChar txt)
                 ""
  where findChar c = 
          case lookup c (mapping font) of
            Nothing -> error $ "No data for char: " ++ [c]
            Just m -> m
        poinToChar p = 
          case p of
            W -> ' '
            B -> filler font
        concatSegments ((s:ss):ls) acc = 
          concatSegments (ls ++ [ss])
                         (acc ++ map poinToChar s)
        concatSegments _ acc = acc
