module Recipe.Recipe where

import Data.List
import Recipe.Tree

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

data Recipe = Ingredient String
            | Heat Temperature Recipe
            | Wait Int
            | Combine Recipe Recipe
            | Sequence Recipe Recipe
            -- | forall a. Eq a => Recipe `Until` a
            deriving Show

type Quantity = Int
type Time = Int

data Temperature = Deg Int | Low | Medium | High
    deriving (Show, Eq)

-- With adding the Measure combinator we may be able to extract
-- any quantifiable combinators into the measure combinator
-- e.g. Wait is a quantity of time, heat int recipe is a quantity of heat

-- measure :: Quantity -> Recipe -> Recipe
-- measure = Measure

heat :: Temperature -> Recipe -> Recipe
heat = Heat

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

-- r1 then r2
(>>>) :: Recipe -> Recipe -> Recipe
(>>>) = Sequence

wait :: Quantity -> Recipe
wait = Wait

-------------------------------------
-- RECIPE SEMANTICS
-------------------------------------

data Action =
    Get String
    -- Heat
    | Preheat Temperature
    | Refrigerate
    | PlaceInHeat
    | LeaveRoomTemp
    | Freeze
    -- Wait
    | DoNothing Time
    -- Combine
    | PlaceAbove
    | PlaceIn
    | PourOver
    | Mix
    deriving Show

-- Translate Recipe into a tree of actions
expand :: Recipe -> Tree Action
expand r@(Ingredient s)   = Node (Get s) []
expand r@(Heat t r')      = Node PlaceInHeat [Node (Preheat t) [], expand r']
expand r@(Wait t)         = Node (DoNothing t) []
expand r@(Combine r1 r2)  = Node Mix [expand r1, expand r2]
expand r@(Sequence r1 r2) = Node (DoNothing 0) [expand r1, expand r2]

-- filter topological sorts using state
-- transpose filtered list 
-- !! time with the transposed list

type RP = RS -> RA

data RS = RS
    { rsTime :: Time
    , rsProgress :: [Label]
    }
    deriving Show
    
type RA = [Label]
type TSort = [Label]

process :: [TSort] -> RP
process ts = \RS{rsTime = time, rsProgress = prog} ->
    let opts = transpose ords
        ords = filter (\x -> take (length prog) x == prog) ts
    in opts `safeIx` time

safeIx :: [[a]] -> Int -> [a]
xs `safeIx` i
    | i >= length xs = []
    | otherwise      = xs !! i

-------------------------------------
-- CONCRETE IMPLEMENTATION
-------------------------------------

-- TODO

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

-- Create a list of ingredients used in a recipe
getIngredients :: Recipe -> [String]
getIngredients (Ingredient s)   = [s]
getIngredients (Heat _ r)       = getIngredients r
getIngredients (Combine r1 r2)  = getIngredients r1 ++ getIngredients r2
getIngredients (Wait _)         = []
getIngredients (Sequence r1 r2) = getIngredients r1 ++ getIngredients r2