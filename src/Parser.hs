module Parser (parseBdfFont) where

import qualified Data.Char as C
import qualified Data.Bits as B

import Types
import Numeric

parseBdfFont :: String -- ^ Font file content
             -> Char   -- ^ Filler
             -> Font   -- ^ Parsed font
parseBdfFont content filler = 
  let ls = lines content
      (size,charsInfo) = lookupSize ls
      chars = lookupChars charsInfo size []
  in Font size filler $ filter (\(c,_) -> C.isPrint c && C.isAscii c) chars

lookupChars :: [String]       -- ^ font file lines
            -> Int            -- ^ font size
            -> [(Char,Glyph)] -- ^ result accumulated so far
            -> [(Char,Glyph)] -- ^ result
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
            (map (hexToGlyphRow size) (take size ls)
            ,drop size ls)
          _ -> readBitmap ls
  in case lookupEncoding ls of
       Nothing -> acc
       Just (code,bitmap) -> 
         lookupChars left
                     size
                     ((C.chr code,bits) : acc)
         where (bits,left) = readBitmap bitmap

hexToGlyphRow :: Int      -- ^ letter width
              -> String   -- ^ hex representation
              -> GlyphRow -- ^ lette row
hexToGlyphRow width hex = case (readHex hex :: [(Integer, String)]) of
  [] -> error $ "Ivalid hex value: " ++ hex
  ((v,_):_) -> map (\p -> if B.testBit v p then B else W) $ reverse [0 .. width-1]

lookupSize :: [String]        -- ^ font file lines
           -> (Int,[String]) -- ^ size and lines left to parse
lookupSize [] = error "No size defined in font"
lookupSize (l:ls) = 
  case readSize l of
    Just s -> (s,ls)
    Nothing -> lookupSize ls
  where readSize ('S':'I':'Z':'E':' ':r) = Just $ read $ takeWhile C.isDigit r
        readSize _ = Nothing
