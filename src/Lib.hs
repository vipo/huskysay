module Lib
where

import Types
import Defaults

-- | Renders a given text as ascii art
render :: String -- ^ String to render
       -> Font   -- ^ Font to use       
       -> String -- ^ Lines of rendered text 
render txt font = concatSegments ((map findChar txt) ++ [newlines]) ""
  where
    findChar c = case lookup c (mapping font) of
      Nothing -> error $ "No data for char: " ++ [c]
      Just m -> m
    newlines = replicate (size font) "\n"
    concatSegments ((s:ss):ls) acc = concatSegments (ls ++ [ss]) (acc ++ s)
    concatSegments _ acc = acc
