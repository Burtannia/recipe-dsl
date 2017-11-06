{-# LANGUAGE ExistentialQuantification #-}

module Recipe.Recipe where

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
--             | Optional Recipe
--             | Temporary Recipe
--             | Measure Quantity Recipe
--             | Combine CMethod Recipe Recipe
--             | Sequence Recipe Recipe

-- data CMethod = Mix | Stack | PlaceIn

-- instance Monoid Recipe where
--     mempty  = Void
--     mappend = (><)

data Recipe = Ingredient String
            | Heat Int Recipe
            | Wait Int
            | Combine Recipe Recipe
            | Sequence Recipe Recipe
            -- | forall a. Condition a => Recipe `Until` a

type Quantity = Int
type Time = Int

-- time, temperature, image from AI camera
class Condition a where
    eval :: a -> (a -> Bool)

newtype Temperature = Temp Int
    deriving (Eq, Show)

instance Condition Temperature where
    eval = (\x -> (==) x)

-- With adding the Measure combinator we can extract
-- any quantifiable combinators into the measure combinator
-- e.g. Wait is a quantity of time, heat int recipe is a quantity of heat

heat :: Int -> Recipe -> Recipe
heat = Heat

-- optional :: Recipe -> Recipe
-- optional = Optional

-- temporary :: Recipe -> Recipe
-- temporary = Temporary

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

-- type TimerId = Int
-- type StartTime = Int
-- type Timer = (TimerId, StartTime)
-- type TimerLog = [Timer]

-- newTimer :: TimerId -> TimerLog -> TimerLog
-- newTimer i l = (i, currentTime) : l

-- getTimerVal :: TimerId -> TimerLog
-- getTimerVal i ts = case [t | t <- ts, fst t == i] of
--     [] -> 0
--     xs -> currentTime - (snd $ head xs)

-- currentTime :: Int
-- currentTime = 100 -- get system time or something

-- RP: Recipe Process - how to perform the Recipe. Translates Recipe into a set of fundamental actions.

-- Recipes are compositional, to get the Actions for a Recipe
-- compute Actions for sub recipes and combine in combinator dependent way

-- RT: Recipe Time - sum of time of Actions

-- RC: Recipe cost

-- E : Recipe -> RP
-- RP = [Conditions] -> RA
-- RA = [Action] -- leaves of Recipe tree at that point in the Recipe
-- Recipe is a process that models the set of actions you could be doing at a given stage
-- This set becomes smaller as time progresses

-- e.g. cupOfTea
-- could start by getting milk and measuring it
-- could get water and boil it
-- could get teabag

-- by the end of the recipe the only thing we can do
-- is combine the tea and the milk

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

-------------------------------------
-- CONCRETE IMPLEMENTATION
-------------------------------------

-- Concrete implementation: various simulation models e.g. professional kitchen with brigade
-- data Kitchen = Kitchen
--     { kActions :: Recipe -> (Time -> Action)
--     , kPaths   :: Int -- number of paths of concurrent execution
--     }
