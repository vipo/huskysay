module Defaults
where

import Types
import qualified Data.Map.Strict as M

defaultFont :: Font
defaultFont = Font { size = 12 , mapping = M.empty }

