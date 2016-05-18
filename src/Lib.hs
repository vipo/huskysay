module Lib where

import qualified Data.Bits as B

import Types
import Defaults

-- | Renders a given text as ascii art
render :: String -- ^ String to render
       -> Font   -- ^ Font to use       
       -> String -- ^ Lines of rendered text 
render txt font = 
  concatSegments (map findChar txt) ""
  where findChar c = 
          case lookup c (mapping font) of
            Nothing -> error $ "No data for char: " ++ [c]
            Just m -> m
        newlines = replicate (size font) "\n"
        integerToSegment i = map (\p -> if B.testBit i p then (filler font) else ' ') $ reverse [0 .. size font -1]
        concatSegments ((s:ss):ls) acc = 
          concatSegments (ls ++ [ss])
                         (acc ++ integerToSegment s)
        concatSegments _ acc = acc
