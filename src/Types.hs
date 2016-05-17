module Types
where

import qualified Data.Map.Strict as M

type CharData = [String]

data Font = Font {
    size :: Int
  , mapping :: M.Map Char CharData
}
