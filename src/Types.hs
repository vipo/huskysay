module Types
where

data Font = Font {
    size :: Int
  , mapping :: [(Char, [String])]
} deriving Show
