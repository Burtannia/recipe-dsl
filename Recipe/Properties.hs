{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe

type PropertySet k v = [(k, v)]

vals :: [(k,v)] -> [v]
vals = map snd

concatVals :: Monoid v => [(k,v)] -> v
concatVals = mconcat . vals

evalProperties :: (Eq k, Monoid v, Foldable t) => PropertySet k v -> t k -> v
evalProperties ps =
    let valFor x = concatVals $ filter (\(k,v) -> k == x) ps
     in foldr (\x v -> valFor x `mappend` v) mempty

evalPropertiesQ :: (Eq k, Integral v, Monoid v, Foldable t) => PropertySet k (v, Measurement) -> t (k, Measurement) -> v
evalPropertiesQ ps =
    let valsFor (x,m) = vals $ filter (\(k,v) -> k == x) ps
        quantValFor (x,m) = mconcat $ map (applyQuant m) (valsFor (x,m))
     in foldr (\x v -> quantValFor x `mappend` v) mempty

applyQuant :: Integral v => Measurement -> (v, Measurement) -> v
applyQuant m (v,m') =
    let r = ratio m m'
        v' = fromIntegral v
     in round $ r * v'

ratio :: Measurement -> Measurement -> Float
ratio (Count i) (Count i') = ratio' i i'
ratio (Grams i) (Grams i') = ratio' i i'
ratio (Milliletres i) (Milliletres i') = ratio' i i'
ratio _ _ = 0.0

ratio' :: Int -> Int -> Float
ratio' i i' = if i' == 0
    then 0.0
    else fromIntegral i / fromIntegral i'

-------------------------------------
-- PRICE
-------------------------------------

newtype Price = Price { pence :: Int }
    deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Price where
    show Price {..} =
        let pounds = pence `div` 100
            pence' = pence `mod` 100
         in '£' : show pounds ++ '.' : show pence'

instance Monoid Price where
    mempty = Price 0
    mappend = (+)