module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe

lookupProperties :: Monoid m => Map Action m -> Recipe -> m
lookupProperties pMap (Node a ts) =
    let val = case Map.lookup a pMap of
                Just v -> v
                Nothing -> mempty
     in foldr (mappend . lookupProperties pMap) val ts