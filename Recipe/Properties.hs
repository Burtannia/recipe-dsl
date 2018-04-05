{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Recipe.Properties where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree
import           Recipe.Recipe

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

type Cuisine = PropertyList FoodType -> [String]

meatTwoVeg :: Cuisine
meatTwoVeg is =
    let ms = filter (\(_,t) -> t == Meat) is
        vs = filter (\(_,t) -> t == Veg) is
     in map fst $ head ms : take 2 vs

mkRecipe :: [String] -> PropertyList Recipe -> Recipe
mkRecipe is rs =
    let parts = map (\s -> case lookup s rs of
                            Just r -> r
                            Nothing -> ingredient s) is
     in foldr1 (combine "next to") parts

-------------------------------------
-- Seasoning
-------------------------------------

data Seasoning = Salt | Sugar | Acid