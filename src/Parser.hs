module Parser (parseBdfFont) where

import Types
import Data.Char
import Numeric

parseBdfFont :: String -- ^ Font file content
             -> Char   -- ^ Filler
             -> Font   -- ^ Parsed font
parseBdfFont content filler = 
  let ls = lines content
      (size,charsInfo) = lookupSize ls
      chars = lookupChars charsInfo size []
  in Font size filler $ filter (\(c,_) -> isPrint c && isAscii c) chars

lookupChars :: [String]           -- ^ font file lines
            -> Int                -- ^ font size
            -> [(Char,[Integer])] -- ^ result accumulated so far
            -> [(Char,[Integer])] -- ^ result
lookupChars [] _ acc = acc
lookupChars ls size acc = 
  let lookupEncoding [] = Nothing
      lookupEncoding (l:ls) = 
        case l of
          ('E':'N':'C':'O':'D':'I':'N':'G':' ':r) -> Just (read r,ls)
          _ -> lookupEncoding ls
      readBitmap [] = error "Bitmap error"
      readBitmap (l:ls) = 
        case l of
          ('B':'I':'T':'M':'A':'P':_) -> 
            (map hexToInt (take size ls)
            ,drop size ls)
          _ -> readBitmap ls
  in case lookupEncoding ls of
       Nothing -> acc
       Just (code,bitmap) -> 
         lookupChars left
                     size
                     ((chr code,bits) : acc)
         where (bits,left) = readBitmap bitmap

hexToInt :: String  -- ^ hex representation
         -> Integer -- ^ rendered char representation
hexToInt hex = case readHex hex of
  [] -> error $ "Ivalid hex value: " ++ hex
  ((v,_):_) -> v

lookupSize :: [String]        -- ^ font file lines
           -> (Int,[String]) -- ^ size and lines left to parse
lookupSize [] = error "No size defined in font"
lookupSize (l:ls) = 
  case readSize l of
    Just s -> (s,ls)
    Nothing -> lookupSize ls
  where readSize ('S':'I':'Z':'E':' ':r) = Just $ read $ takeWhile isDigit r
        readSize _ = Nothing
