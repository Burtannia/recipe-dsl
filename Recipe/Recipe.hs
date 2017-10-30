module Recipe.Recipe where

-------------------------------------
-- TEST RECIPES
-------------------------------------

milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"

cupOfTea :: Recipe
cupOfTea = milk >< ((teabag >< (water `heatAt` 100)) >>> wait 5)

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
tomatoSauce = (((((oliveOil `heatAt` 2) >< onion >< garlic) >>> wait 5)
              >< tinnedTomatoes >< measure 6 tomato >< vegStock >< bayLeaf >< thyme)
              >>> wait 20) >< basil

chilliConCarne :: Recipe
chilliConCarne = ((((((((oliveOil `heatAt` 2) >< beefMince) >>> wait 5)
                    >< cumin >< paprika >< cayenne) >>> wait 5)
                    >< tomatoSauce >< beefStock) >>> wait 30)
                    >< kidneyBeans >< redChilli >< coriander) >>> wait 10

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

data Recipe = Ingredient String
            | Heat
            | Time
            | Void
            | Optional (Bool -> Recipe)
            | Measure Quantity Recipe
            | Combine Recipe Recipe
            | Sequence Recipe Recipe

type Quantity = Int

-- With adding the Measure combinator we can extract
-- any quantifiable combinators into the measure combinator
-- e.g. Wait is a quantity of time, heat int recipe is a quantity of heat

optional :: Recipe -> Recipe
optional r = Optional (\b -> if b
                               then r
                               else Void)

measure :: Quantity -> Recipe -> Recipe
measure = Measure

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

-- r1 then r2
(>>>) :: Recipe -> Recipe -> Recipe
(>>>) = Sequence

-- Can now build up some new combinators

heatAt :: Recipe -> Quantity -> Recipe
r `heatAt` t = (measure t Heat) >< r

forDuration :: Recipe -> Quantity -> Recipe
r `forDuration` q = (measure q Time) >< r

wait :: Quantity -> Recipe
wait q = (measure q Time) >< Void

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

-- Create a list of steps of the Recipe
extractSteps :: Recipe -> [LabelledRecipe]
extractSteps = extractStepsL . labelRecipe

-- Create a list of steps of a LabelledRecipe
extractStepsL :: LabelledRecipe -> [LabelledRecipe]
extractStepsL x@(LOptional _ r)    = extractStepsL (r True) ++ [x]
extractStepsL x@(LMeasure _ _ r)   = extractStepsL r ++ [x]
extractStepsL x@(LCombine _ r1 r2) = extractStepsL r1 ++ extractStepsL r2 ++ [x]
extractStepsL (LSequence r1 r2)    = extractStepsL r1 ++ extractStepsL r2
extractStepsL _                    = []

-------------------------------------
-- RECIPE LABELLING
-------------------------------------

-- r' is a LabelledRecipe therefore we can't construct
-- Optional r'.

-- labelRecipe' l (Optional r)     = (l', Optional r')
-- where
--   r' = labelRecipe' l r
--   l' = calcLabel l r'

data LabelledRecipe = LIngredient String
                    | LHeat
                    | LTime
                    | LVoid
                    | LOptional Label (Bool -> LabelledRecipe)
                    | LMeasure Label Quantity LabelledRecipe
                    | LCombine Label LabelledRecipe LabelledRecipe
                    | LSequence LabelledRecipe LabelledRecipe

type Label = Int

getLabel :: LabelledRecipe -> Label
getLabel (LOptional l _)  = l
getLabel (LMeasure l _ _) = l
getLabel (LCombine l _ _) = l
getLabel _                = 0

labelRecipe :: Recipe -> LabelledRecipe
labelRecipe = labelRecipe' 1

labelRecipe' :: Label -> Recipe -> LabelledRecipe
labelRecipe' _ (Ingredient s)   = LIngredient s
labelRecipe' _ Heat             = LHeat
labelRecipe' _ Time             = LTime
labelRecipe' _ Void             = LVoid

labelRecipe' l (Optional br)    = LOptional l' (\b -> if b
                                                        then r'
                                                        else LVoid)
                                  where
                                    r  = br True
                                    r' = labelRecipe' l r
                                    l' = calcLabel l r'

labelRecipe' l (Measure q r)    = LMeasure l' q r'
                                  where
                                    r' = labelRecipe' l r
                                    l' = calcLabel l r'

labelRecipe' l (Combine r1 r2)  = LCombine l'' lr1 lr2
                                  where
                                    lr1 = labelRecipe' l r1
                                    lr2 = labelRecipe' l' r2
                                    l'  = calcLabel l lr1
                                    l'' = calcLabel l' lr2

labelRecipe' l (Sequence r1 r2) = LSequence lr1 lr2
                                  where
                                    lr1 = labelRecipe' l r1
                                    lr2 = labelRecipe' l' r2
                                    l'  = calcLabel l lr1

calcLabel :: Label -> LabelledRecipe -> Label
calcLabel l r' = case r' of
    (LIngredient _)  -> l
    LHeat            -> l
    LTime            -> l
    LVoid            -> l
    (LSequence _ r2) -> getLabel r2 + 1
    _                -> getLabel r' + 1

-------------------------------------
-- RECIPE SEMANTICS
-------------------------------------

-- executeR :: Recipe -> (World -> Action)
-- executeR r = case r of
--     Ingredient s -> (\w ->
--         if not $ r `elem` w
--             then Get r
--             else Idle)

--     Heat         -> (\_ -> Idle)
--     Time         -> (\_ -> Idle)
--     Void         -> (\_ -> Idle)

--     Optional br -> (\w ->
--         if Decide (br True) True `elem` w
--             then executeR (br True)
--             else if Decide (br True) False `elem` w
--                     then Idle
--                     else Decide (br True)

--     Measure q r -> (\w ->
--         )

-- type World = [Action]

-- Concrete implementation: various simulation models e.g. professional with brigade
-- data Kitchen = Kitchen
--     { kActions :: Recipe -> (Time -> Action)
--     , kPaths   :: Int -- number of paths of concurrent execution
--     }

-- dice r = Diced r (describes what we want)
-- this has to compile into a set of cutting instructions
-- Cut Method (0 to 1)
