module Recipe.Recipe where

-------------------------------------
-- TEST RECIPES
-------------------------------------

milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"

cupOfTea :: Recipe
cupOfTea = milk >< ((teabag >< heat (temp 100) water) >>> wait 5)

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

chilliConCarne :: Recipe
chilliConCarne = (((((((heat Medium oliveOil >< beefMince) >>> wait 5)
                    >< cumin >< paprika >< cayenne) >>> wait 5)
                    >< tomatoSauce >< beefStock) >>> wait 30)
                    >< kidneyBeans >< redChilli >< coriander) >>> wait 10

tomatoSauce :: Recipe
tomatoSauce = ((((heat Medium oliveOil >< onion >< garlic) >>> wait 5)
                >< tinnedTomatoes >< tomato >< vegStock >< bayLeaf >< thyme)
                >>> wait 20) >< basil -- need to remove bayLeaf

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

type Time = Int

data Temperature = Degrees Int | High | Medium | Low
    deriving Show

data Recipe = Ingredient String
            | Heat Temperature Recipe
            | Combine Recipe Recipe
            | Wait Time
            | Sequence Recipe Recipe
            deriving (Show)

temp :: Int -> Temperature
temp = Degrees

heat :: Temperature -> Recipe -> Recipe
heat = Heat

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

wait :: Time -> Recipe
wait = Wait

-- r1 then r2
(>>>) :: Recipe -> Recipe -> Recipe
(>>>) = Sequence

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
                    | LHeat Label Temperature LabelledRecipe
                    | LCombine Label LabelledRecipe LabelledRecipe
                    | LWait Label Time
                    | LSequence LabelledRecipe LabelledRecipe
                    deriving (Show)

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

labelRecipe' l (Wait t)         = LWait l t

labelRecipe' l (Sequence r1 r2) = LSequence lr1 lr2
                                  where
                                    lr1 = labelRecipe' l r1
                                    lr2 = labelRecipe' l' r2
                                    l'  = calcLabel l lr1
                                    l'' = calcLabel l' lr2

calcLabel :: Label -> LabelledRecipe -> Label
calcLabel l r' = case r' of
    (LIngredient s)  -> l
    (LSequence _ r2) -> getLabel r2 + 1
    _                -> getLabel r' + 1

-------------------------------------
-- MORE COMPLEX DEFINITIONS
-------------------------------------

-- data Quantity = Measurement Int | Fraction Int Int
--
-- instance Show Quantity where
--         show (Measurement i) = show i
--         show (Fraction n d) = "take " ++ show n ++ "//" ++ show d ++ " of the recipe"
--
-- data Position = Beside | Above | Wrap
--
-- instance Show Position where
-- 	show Beside = "and place it next to"
-- 	show Above = "and place it on top of"
-- 	show Wrap = "and wrap it in"

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Prepare Recipe -- cut, slice, peel etc.
--             | Wait Time
--             | Assemble Position Recipe Recipe
--             | After Recipe Recipe
--             | Measure Quantity Recipe
--             deriving (Show)

-- prepare :: Recipe -> Recipe
-- prepare = Prepare
--
-- measure :: Quantity -> Recipe -> Recipe
-- measure = Measure
--
-- -- assemble vertically (r1 above r2)
-- (^^^) :: Recipe -> Recipe -> Recipe
-- r1 ^^^ r2 = Assemble Above r1 r2
--
-- -- assemble horizontally
-- (~~~) :: Recipe -> Recipe -> Recipe
-- r1 ~~~ r2 = Assemble Beside r1 r2
--
-- -- wrap r2 in r1
-- (@@@) :: Recipe -> Recipe -> Recipe
-- r1 @@@ r2 = Assemble Wrap r1 r2

-- Must consider how to deal with splitting a recipe, at the moment it would be something like this:
-- (orangeZest >< (measure (Fraction 1 2) cakeMix)) ^^^ (lemonZest >< (measure (Fraction 1 2) cakeMix)
