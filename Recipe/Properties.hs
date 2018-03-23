module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe

-- Monoid m => Map k m

lookupProperties :: Monoid m => Map Label m -> Tree Label -> m
lookupProperties pMap lTree = undefined
