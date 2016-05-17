module Types
where

type CharData = [String]

data Font = Font {
    size :: Int
  , mapping :: [(Char, CharData)]
} deriving Show
