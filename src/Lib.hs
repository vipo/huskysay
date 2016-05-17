module Lib
where

import Types
import Defaults

import Numeric
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Bits as B

-- | Renders a given text as ascii art
render :: String   -- ^ String to render
       -> Font     -- ^ Font to use       
       -> [String] -- ^ Lines of rendered text 
render txt font = [txt]

parseFont :: String -- ^ Font file content
          -> Font   -- ^ Parsed font
parseFont content =
  let
    ls = lines content
    (size, charsInfo) = lookupSize ls
    chars = lookupChars charsInfo size []
  in 
    Font {
        size = size
      , mapping = M.fromList (filter (\(c, _) -> isPrint c && isAscii c) chars)
    }

lookupChars :: [String]           -- ^ font file lines
            -> Int                -- ^ font size
            -> [(Char, [String])] -- ^ result accumulated so far
            -> [(Char, [String])] -- ^ Char to rendered lines mapping, and lines to parse left
lookupChars [] _ acc = acc
lookupChars ls size acc =
  let
    lookupEncoding [] = error "Cannot find encoding"
    lookupEncoding (l:ls) = case l of
      ('E':'N':'C':'O':'D':'I':'N':'G':' ':r) -> read r
      _ -> lookupEncoding ls
    readBitmap [] = error "Bitmap error"
    readBitmap (l:ls) = case l of
      ('B':'I':'T':'M':'A':'P':_) -> (map (renderFromHex size) (take size ls), drop size ls)
      _ -> readBitmap ls
    (code, bitmap) = lookupEncoding ls
    (bits, left) = readBitmap bitmap
  in
    lookupChars left size ((chr code, bits):acc)

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
