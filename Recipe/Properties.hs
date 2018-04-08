{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe
import Data.Maybe (fromJust, catMaybes)
import Data.List (maximumBy)

type PropertyList v = [(String, v)]

lookupProp :: Monoid v => PropertyList v -> String -> v
lookupProp ps s = case lookup s ps of
    Nothing -> mempty
    Just v -> v

evalProps :: Monoid v => PropertyList v -> Recipe -> v
evalProps ps = foldRecipe f
    where
        f (GetIngredient s) = lookupProp ps s
        f _ = mempty        

evalPropsQ :: (Integral v, Monoid v) => PropertyList (v, Measurement) -> [(String, Measurement)] -> v
evalPropsQ ps xs = mconcat $ map evalPropsQ' xs
    where
        evalPropsQ' (s,m) = case lookup s ps of
            Nothing -> mempty
            Just (v,m') -> applyQuant m (v,m')

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
            showPence p
                | p < 10 = '0' : show p
                | otherwise = show p
         in '£' : show pounds ++ '.' : showPence pence'
        
instance Monoid Price where
    mempty = Price 0
    mappend = (+)

type PriceList = PropertyList (Price, Measurement)

price :: PriceList -> Recipe -> Price
price ps r = evalPropsQ ps (ingredientsQ r)

-------------------------------------
-- Food Type
-------------------------------------

data FoodType = Meat | Veg
    deriving (Show, Eq)

type Cuisine = PropertyList FoodType -> PropertyList Recipe -> Recipe

-------------------------------------
-- Seasoning
-------------------------------------

data Seasoning = Salt | Sweet | Acid
    deriving (Eq, Ord, Show)

type Profile = Map Seasoning Int

profile :: [Seasoning] -> Profile
profile [] = Map.fromList [(Salt, 0), (Sweet, 0), (Acid, 0)]
profile (s:ss) =
    let m = profile ss
        i = fromJust $ Map.lookup s m
     in Map.insert s (i+1) m

balance :: Recipe -> PropertyList Seasoning -> [(Seasoning, Int)]
balance r ss =
    let is = ingredients r
        flavours = catMaybes $ map (\i -> lookup i ss) is
        p = Map.toList $ profile flavours
        (_,maxI) = maximumBy (\(_,i) (_,i') -> compare i i') p
     in case p of
            [(Salt, 0), (Sweet, 0), (Acid, 0)] -> map (\(s,_) -> (s,1)) p
            _ -> [(s, maxI - i) | (s,i) <- p]


