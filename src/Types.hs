module Types where

data Font =
  Font {size :: Int
       ,filler :: Char
       ,mapping :: [(Char,[Integer])]}
  deriving ((((Show))))
