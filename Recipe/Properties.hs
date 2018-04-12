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

-- |List of String keys and properties
-- of a given type.
type PropertyList v = [(String, v)]

-- |Lookup the property of a given key in the PropertyList.
-- Returns mempty if not found.
lookupProp :: Monoid v => PropertyList v -> String -> v
lookupProp ps s = case lookup s ps of
    Nothing -> mempty
    Just v -> v

-- |Cumulatively, evaluates the properties of every ingredient in the
-- given recipe.
evalProps :: Monoid v => PropertyList v -> Recipe -> v
evalProps ps = foldRecipe f
    where
        f (GetIngredient s) = lookupProp ps s
        f _ = mempty        

-- |The same as evalProps but considers the quantity of an ingredient used
-- and calculates the ratio accordingly.
evalPropsQ :: (Integral v, Monoid v) => PropertyList (v, Measurement) -> [(String, Measurement)] -> v
evalPropsQ ps xs = mconcat $ map evalPropsQ' xs
    where
        evalPropsQ' (s,m) = case lookup s ps of
            Nothing -> mempty
            Just (v,m') -> applyQuant m (v,m')

-- |Apply the given measurement to the given quantified property.
-- Returning the value of the property applicable to the given measurement.
-- Informally: applyQuant 100g (£1,50g) means "Every 50g costs £1,
-- I have 100g therefore that costs £2".
applyQuant :: Integral v => Measurement -> (v, Measurement) -> v
applyQuant m (v,m') =
    let r = ratio m m'
        v' = fromIntegral v
     in round $ r * v'

-- |The ratio of two measurements.
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

-- |Price is a wrapper around an Int representing pence.
newtype Price = Price { pence :: Int }
    deriving (Eq, Ord, Num, Real, Enum, Integral)

-- |Price shown as £P.pp.
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

-- |PropertyList mapping an ingredient (String) to
-- a pair of price per measurement.
type PriceList = PropertyList (Price, Measurement)

-- |Given a price list, calculate the price of a recipe.
price :: PriceList -> Recipe -> Price
price ps r = evalPropsQ ps (ingredientsQ r)

-------------------------------------
-- Cuisine
-------------------------------------

type Cuisine a = PropertyList a -> Recipe

-------------------------------------
-- Seasoning
-------------------------------------

-- |Representation of the Salt, Sweet, Acid seasoning triangle.
data Seasoning = Salt | Sweet | Acid
    deriving (Eq, Ord, Show)

-- |A profile is a mapping of a seasoning to the number of
-- times it occurs.
type Profile = Map Seasoning Int

-- |Given a list of seasonings, produce a profile of their
-- frequencies.
profile :: [Seasoning] -> Profile
profile [] = Map.fromList [(Salt, 0), (Sweet, 0), (Acid, 0)]
profile (s:ss) =
    let m = profile ss
        i = fromJust $ Map.lookup s m
     in Map.insert s (i+1) m

-- |Given a recipe and a property list of seasonings, find the
-- seasoning that each ingredient offers. Then profile said seasonings
-- and return a list showing how much each seasoning needs to be
-- increased by to balance the recipe. For example if a recipe
-- had a profile of [(Salt, 1), (Sweet, 0), (Acid, 2)] then 'balance'
-- would return [(Salt, 1), (Sweet, 2), (Acid, 0)].
balance :: Recipe -> PropertyList Seasoning -> [(Seasoning, Int)]
balance r ss =
    let is = ingredients r
        flavours = catMaybes $ map (\i -> lookup i ss) is
        p = Map.toList $ profile flavours
        (_,maxI) = maximumBy (\(_,i) (_,i') -> compare i i') p
     in case p of
            [(Salt, 0), (Sweet, 0), (Acid, 0)] -> map (\(s,_) -> (s,1)) p
            _ -> [(s, maxI - i) | (s,i) <- p]


