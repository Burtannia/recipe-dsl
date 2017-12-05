{-# LANGUAGE ExistentialQuantification #-}

module Recipe.Recipe where

import Data.List

-------------------------------------
-- TEST RECIPES
-------------------------------------

milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"

cupOfTea :: Recipe
cupOfTea = milk >< ((teabag >< (heat 100 water)) >>> wait 5)

oliveOil, tomato, onion, garlic, tinnedTomatoes, bayLeaf, thyme, basil, vegStock :: Recipe
oliveOil = Ingredient "olive oil"
tomato = Ingredient "tomato"
onion = Ingredient "onion"
garlic = Ingredient "garlic"
tinnedTomatoes = Ingredient "tinned tomatoes"
bayLeaf = Ingredient "bay leaf"
thyme = Ingredient "thyme"
basil = Ingredient "basil"
vegStock = Ingredient "vegetable stock"

beefMince, cayenne, paprika, cumin, beefStock, kidneyBeans, redChilli, coriander :: Recipe
beefMince = Ingredient "beef mince"
cayenne = Ingredient "cayenne pepper"
paprika = Ingredient "paprika"
cumin = Ingredient "cumin"
beefStock = Ingredient "beef stock"
kidneyBeans = Ingredient "kidney beans"
redChilli = Ingredient "red chilli"
coriander = Ingredient "coriander"

tomatoSauce :: Recipe
tomatoSauce = (((((heat 2 oliveOil) >< onion >< garlic) >>> wait 5)
              >< tinnedTomatoes >< tomato >< vegStock >< bayLeaf >< thyme)
              >>> wait 20) >< basil

chilliConCarne :: Recipe
chilliConCarne = ((((((((heat 2 oliveOil) >< beefMince) >>> wait 5)
                    >< cumin >< paprika >< cayenne) >>> wait 5)
                    >< tomatoSauce >< beefStock) >>> wait 30)
                    >< kidneyBeans >< redChilli >< coriander) >>> wait 10

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

-- data Base = Ingredient String | Intangible

-- data Intangible = Heat | Time | Void

-- data Recipe = Ingredient String
--             | Heat | Time | Void
--             | Measure Quantity Recipe
--             | Combine CMethod Recipe Recipe
--             | Sequence Recipe Recipe

-- need some sort of Remove / Temporary Recipe

-- data CMethod = Mix | Stack | PlaceIn

-- instance Monoid Recipe where
--     mempty  = Void
--     mappend = (><)

data Recipe = Ingredient String
            | Heat Int Recipe
            | Wait Int
            | Combine Recipe Recipe
            | Sequence Recipe Recipe
            -- | forall a. Eq a => Recipe `Until` a
            deriving Show

type Quantity = Int
type Time = Int

-- time, temperature, image from AI camera
class Condition a where
    eval :: a -> (a -> Bool)

newtype Temperature = Temp Int
    deriving (Eq, Show)

instance Condition Temperature where
    eval = (==)

-- With adding the Measure combinator we can extract
-- any quantifiable combinators into the measure combinator
-- e.g. Wait is a quantity of time, heat int recipe is a quantity of heat

heat :: Int -> Recipe -> Recipe
heat = Heat

-- measure :: Quantity -> Recipe -> Recipe
-- measure = Measure

-----------------
--- COMBINES: ---
-----------------

(><) :: Recipe -> Recipe -> Recipe
--(><) = Combine Mix
(><) = Combine

-- (^^^) :: Recipe -> Recipe -> Recipe
-- (^^^) = Combine Stack

-- (@@@) :: Recipe -> Recipe -> Recipe
-- (@@@) = Combine PlaceIn

-----------------

-- r1 then r2
(>>>) :: Recipe -> Recipe -> Recipe
(>>>) = Sequence

-- Can now build up some new combinators

-- heatAt :: Recipe -> Quantity -> Recipe
-- r `heatAt` t = (measure t Heat) >< r

-- forDuration :: Recipe -> Quantity -> Recipe
-- r `forDuration` q = (measure q Time) >< r

wait :: Quantity -> Recipe
--wait q = (measure q Time) >< Void
wait = Wait

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

-- Create a list of steps of the Recipe
extractSteps :: Recipe -> [LabelledRecipe]
extractSteps = extractStepsL . labelRecipe

-- Create a list of steps of a LabelledRecipe
extractStepsL :: LabelledRecipe -> [LabelledRecipe]
extractStepsL (LIngredient _)      = []
extractStepsL x@(LHeat _ _ r)      = extractStepsL r ++ [x]
extractStepsL x@(LCombine _ r1 r2) = extractStepsL r1 ++ extractStepsL r2 ++ [x]
extractStepsL (LSequence r1 r2)    = extractStepsL r1 ++ extractStepsL r2
extractStepsL r                    = [r]

-- Create a list of ingredients in a recipe
getIngredients :: Recipe -> [String]
getIngredients (Ingredient s)   = [s]
getIngredients (Heat _ r)       = getIngredients r
getIngredients (Combine r1 r2)  = getIngredients r1 ++ getIngredients r2
getIngredients (Wait _)         = []
getIngredients (Sequence r1 r2) = getIngredients r1 ++ getIngredients r2

-------------------------------------
-- RECIPE LABELLING
-------------------------------------

type Label = Int

data LabelledRecipe = LIngredient String
                    | LHeat Label Int LabelledRecipe
                    | LCombine Label LabelledRecipe LabelledRecipe
                    | LWait Label Int
                    | LSequence LabelledRecipe LabelledRecipe
                    deriving (Show)

-- type ST Recipe = State -> (State, Recipe)

getLabel :: LabelledRecipe -> Label
getLabel (LIngredient _)  = 0
getLabel (LHeat l _ _)    = l
getLabel (LCombine l _ _) = l
getLabel (LWait l _)      = l
getLabel (LSequence _ _)  = 0

labelRecipe :: Recipe -> LabelledRecipe
labelRecipe = labelRecipe' 1

labelRecipe' :: Label -> Recipe -> LabelledRecipe
labelRecipe' _ (Ingredient s)   = LIngredient s

labelRecipe' l (Heat t r)       = LHeat l' t r'
              where
                r' = labelRecipe' l r
                l' = calcLabel l r'

labelRecipe' l (Combine r1 r2)  = LCombine l'' lr1 lr2
              where
                lr1 = labelRecipe' l r1
                lr2 = labelRecipe' l' r2
                l'  = calcLabel l lr1
                l'' = calcLabel l' lr2

labelRecipe' l (Wait t) = LWait l t

labelRecipe' l (Sequence r1 r2) = LSequence lr1 lr2
              where
                lr1 = labelRecipe' l r1
                lr2 = labelRecipe' l' r2
                l'  = calcLabel l lr1

calcLabel :: Label -> LabelledRecipe -> Label
calcLabel l r' = case r' of
    (LIngredient _)  -> l
    (LSequence _ r2) -> getLabel r2 + 1
    _                -> getLabel r' + 1

-------------------------------------
-- RECIPE SEMANTICS
-------------------------------------

-- ns = sorted nodes, zs = zero degree nodes
-- kahn :: Tree a -> [Tree a] -> [Tree a] -> [Tree a]
-- kahn t ns []                  = []
-- kahn (Node r ts) ns (z:zs) = 
--     where
--         ns' = ns ++ [z]

-- Calculate the degree of a node in the tree
calcDegree :: Tree a -> Int
calcDegree (Node _ ts) = length ts

-- Produce a list of nodes with a degree of zero
zeroDegree :: Tree a -> [Tree a]
zeroDegree n@(Node r []) = [n]
zeroDegree (Node r ts)   = concatMap zeroDegree ts

-- Translate Recipe into a tree of actions
expand :: Recipe -> Tree Action
expand r@(Ingredient s)   = Node (Get r) []
expand r@(Heat t r')      = Node (PlaceInHeat r') [Node (Preheat t) [expand r']]
expand r@(Wait t)         = Node DoNothing []
expand r@(Combine r1 r2)  = Node (Mix r1 r2) [expand r1, expand r2]
expand r@(Sequence r1 r2) = Node DoNothing [expand r1, expand r2]

type RP = Time -> RA
type RA = [Action]

data Action = Get Recipe
    -- Heat
    | Preheat Int
    | Refrigerate Recipe
    | PlaceInHeat Recipe --can infer oven or stove from Temp being Medium or 180
    | LeaveRoomTemp Recipe
    | Freeze Recipe
    -- Wait
    | DoNothing
    -- Combine
    | PlaceAbove Recipe Recipe
    | PlaceIn Recipe Recipe
    | PourOver Recipe Recipe
    | Mix Recipe Recipe
    --
    | Init
    deriving Show

-------------------------------------
-- CONCRETE IMPLEMENTATION
-------------------------------------

-- Concrete implementation: various simulation models e.g. professional kitchen with brigade
-- data Kitchen = Kitchen
--     { kStations :: [Station]
--     , kTime :: Time -- current time
--     }

-- data Station = Station
--     { sActions :: [Action] -- actions the station can perform
--     , sTemperature :: Temperature -- current temperature
--     }

-- type SS = [(Step, Action)] -- the action to be performed at each step
-- type RS = [(Station, SS)] -- Recipe Schedule: list of stations and their schedules

-- schedule :: RP -> Kitchen -> RS
