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

filterJust :: [Maybe a] -> [a]
filterJust xs = [x | (Just x) <- xs]

sortRecipe :: Recipe -> [Action]
sortRecipe r = filterJust mas
    where
        t = labelTree $ expand r
        ls = kahn t
        mas = map (\l -> findLabel l t) ls

-- Translate Recipe into a tree of actions
expand :: Recipe -> Tree Action
expand r@(Ingredient s)   = Node (Get r) []
expand r@(Heat t r')      = Node (PlaceInHeat r') [Node (Preheat t) [], expand r']
expand r@(Wait t)         = Node (DoNothing t) []
expand r@(Combine r1 r2)  = Node (Mix r1 r2) [expand r1, expand r2]
expand r@(Sequence r1 r2) = Node (DoNothing 0) [expand r1, expand r2]

type RP = Time -> RA
type RA = [Action]

data Action = Get Recipe
    -- Heat
    | Preheat Temperature
    | Refrigerate Recipe
    | PlaceInHeat Recipe
    | LeaveRoomTemp Recipe
    | Freeze Recipe
    -- Wait
    | DoNothing Time
    -- Combine
    | PlaceAbove Recipe Recipe
    | PlaceIn Recipe Recipe
    | PourOver Recipe Recipe
    | Mix Recipe Recipe
    deriving Show

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