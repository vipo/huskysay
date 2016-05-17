module Lib
where

import Types
import Defaults

import Numeric
import Data.Char
import qualified Data.Bits as B

-- | Renders a given text as ascii art
render :: String -- ^ String to render
       -> Font   -- ^ Font to use       
       -> String -- ^ Lines of rendered text 
render txt font = concatSegments ((map findChar txt) ++ [newlines]) ""
  where
    findChar :: Char -> CharData
    findChar c = case lookup c (mapping font) of
      Nothing -> error $ "No data for char: " ++ [c]
      Just m -> m
    newlines :: CharData
    newlines = replicate (size font) "\n"
    concatSegments :: [CharData] -> String -> String
    concatSegments ((s:ss):ls) acc = concatSegments (ls ++ [ss]) (acc ++ s)
    concatSegments _ acc = acc

parseFont :: String -- ^ Font file content
          -> Font   -- ^ Parsed font
parseFont content =
  let
    ls = lines content
    (size, charsInfo) = lookupSize ls
    chars = lookupChars charsInfo size []
  in 
    Font size $ filter (\(c, _) -> isPrint c && isAscii c) chars

lookupChars :: [String]           -- ^ font file lines
            -> Int                -- ^ font size
            -> [(Char, [String])] -- ^ result accumulated so far
            -> [(Char, [String])] -- ^ Char to rendered lines mapping, and lines to parse left
lookupChars [] _ acc = acc
lookupChars ls size acc =
  let
    lookupEncoding [] = Nothing
    lookupEncoding (l:ls) = case l of
      ('E':'N':'C':'O':'D':'I':'N':'G':' ':r) -> Just (read r, ls)
      _ -> lookupEncoding ls
    readBitmap [] = error "Bitmap error"
    readBitmap (l:ls) = case l of
      ('B':'I':'T':'M':'A':'P':_) -> (map (renderFromHex size) (take size ls), drop size ls)
      _ -> readBitmap ls
  in
    case lookupEncoding ls of
      Nothing -> acc
      Just (code, bitmap) ->
        lookupChars left size ((chr code, bits):acc)
        where
          (bits, left) = readBitmap bitmap

renderFromHex :: Int    -- ^ width
              -> String -- ^ hex representation
              -> String -- ^ rendered char
renderFromHex size hex =
  let
    v = fst $ head $ readHex hex :: Int
  in
    map (\p -> if B.testBit v p then '*' else ' ') ([0 .. size-1] :: [Int])

lookupSize :: [String]        -- ^ font file lines
           -> (Int, [String]) -- ^ size and lines left to parse
lookupSize [] = error "No size defined in font"
lookupSize (l:ls) =
  case readSize l of
    Just s -> (s, ls)
    Nothing -> lookupSize ls
  where
    readSize ('S':'I':'Z':'E':' ':r) = Just $ read $ takeWhile isDigit r
    readSize _ = Nothing
