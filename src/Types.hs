module Types where

data Point = W | B
  deriving (Show)

type GlyphRow = [Point]
type Glyph = [GlyphRow]

data Font =
  Font {size    :: Int
       ,filler  :: Char
       ,mapping :: [(Char,Glyph)]}
  deriving (Show)
