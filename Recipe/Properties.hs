{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe
import Text.Printf

lookupProperties :: Monoid m => Map Action m -> Recipe -> m
lookupProperties pMap =
    let valOf = \a -> case Map.lookup a pMap of
                        Just v -> v
                        Nothing -> mempty
     in foldr (\a m -> valOf a `mappend` m) mempty

-------------------------------------
-- PRICE
-------------------------------------

newtype Price = Price { pence :: Int }
    deriving (Eq, Ord, Num, Real, Enum, Integral)

ppPrice :: Price -> IO ()
ppPrice Price{..} =
    let pndsPence = (fromIntegral pence) / 100
     in printf "%s%.2f\n" "£" (pndsPence :: Float)

instance Monoid Price where
    mempty = Price 0
    mappend = (+)