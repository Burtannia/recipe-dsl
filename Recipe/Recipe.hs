{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Recipe.Recipe where

import           Control.Monad.Trans.State
import           Data.List                 (sort)
import           Data.Tree                 hiding (foldTree)

-------------------------
-- Defining Recipes
-------------------------

-- |Recipe type is simply a Data.Tree of Actions.
type Recipe = Tree Action

-- |Actions represent something that is done during the recipe
-- and are taken to be "applied" on their child nodes in the
-- recipe thus providing sequencing.
-- Not used directly, used via the helper functions.
data Action = GetIngredient String
            | Heat
            | HeatAt Int
            | Wait
            | Combine String
            | Conditional Action Condition
            | Transaction Action
            | Measure Measurement
            deriving (Show, Eq, Ord)

-- |Ordering compares the result of 'deps' for
-- each recipe i.e. the pairs of dependencies
-- for each recipe.
instance Ord Recipe where
    compare r1 r2 = let xs = sort $ deps r1
                        ys = sort $ deps r2
                     in compare xs ys

-- |Two recipes are equal if they contain the same 'Action's
-- and those 'Action's have the same dependencies i.e. the same
-- sequencing.
instance {-# OVERLAPPING #-} Eq Recipe where
    (==) r1 r2 = compare r1 r2 == EQ

-- |'Time' is a wrapper around an Int representing seconds.
newtype Time = Time Int
    deriving (Eq, Ord, Num, Real, Enum, Integral)

-- |'Time' is printed in hms format.
instance Show Time where
    show (Time i) = let h = i `div` 3600
                        m = (i `mod` 3600) `div` 60
                        s = (i `mod` 3600) `mod` 60 in
                    show h ++ "h "
                    ++ show m ++ "m "
                    ++ show s ++ "s"

instance Monoid Time where
    mempty = 0
    mappend = (+)

-- |Conditions represent an event which you perform a recipe "until".
data Condition = CondTime Time -- ^ Until the given time has elapsed.
               | CondTemp Int -- ^ Until the temperature has been reached.
               | CondOpt String -- ^ Optional step, labelled for identification. See observables (Obs).
               | Condition `AND` Condition -- ^ Logical "and" of two conditions.
               | Condition `OR` Condition -- ^ Logical "or" of two conditions.
               deriving (Show, Eq, Ord)

-- |Represents a measurement of something.
data Measurement = Count Int -- ^ Number of something e.g. 1 apple.
                 | Grams Int -- ^ Number of grams.
                 | Milliletres Int -- ^ Number of milliletres.
                 deriving (Eq)

-- |Returns the magnitude of a 'Measurement'.
getMeasure :: Measurement -> Int
getMeasure (Count i)       = i
getMeasure (Grams i)       = i
getMeasure (Milliletres i) = i

-- |Ordering uses the magnitude only.
instance Ord Measurement where
    compare a b = compare (getMeasure a) (getMeasure b)

-- | > show ('Count' 1) = 1
-- > show ('Grams' 100) = 100g
-- > show ('Milliletres' 200) = 200ml
instance Show Measurement where
    show (Count i)       = show i
    show (Grams i)       = show i ++ "g"
    show (Milliletres i) = show i ++ "ml"

-------------------------
-- Creating Recipes
-------------------------

-- |Get an ingredient with the given name.
ingredient :: String -> Recipe
ingredient s = Node (GetIngredient s) []

-- |'Heat' the given recipe, used when the temperature it
-- is heated at doesn't matter for example heating water
-- in a kettle.
heat :: Recipe -> Recipe
heat r = Node Heat [r]

-- |Heat the given recipe at the given temperature. ('HeatAt').
heatAt :: Int -> Recipe -> Recipe
heatAt temp r = Node (HeatAt temp) [r]

-- |'Wait' after performing the given recipe.
-- Without being wrapped with CondTime is an
-- infinitely small wait.
wait :: Recipe -> Recipe
wait r = Node Wait [r]

-- |'Combine' the two recipes using the method given
-- as a String e.g. "mix".
combine :: String -> Recipe -> Recipe -> Recipe
combine s r1 r2 = Node (Combine s) [r1, r2]

-- |Add the given condition to the root action of
-- the given recipe. If the action is already
-- wrapped with a condtion, ('AND') is applied
-- to the two conditions.
addCondition :: Condition -> Recipe -> Recipe
addCondition c (Node a ts) = case a of
    Conditional a' c' -> Node a'' ts
        where a'' = Conditional a' (c .&& c')
    _ -> Node (Conditional a c) ts

-- |Wraps the root node of a recipe in a 'Transaction'.
transaction :: Recipe -> Recipe
transaction (Node a ts) = Node (Transaction a) ts

-- |Adds a 'Measure' action with the given 'Measurement'
-- above the given recipe.
measure :: Measurement -> Recipe -> Recipe
measure m r = Node (Measure m) [r]

-- Nicer Conditions and Time

-- |Applies 'addCondition' 'CondOpt' with the given label.
optional :: String -> Recipe -> Recipe
optional s = addCondition (CondOpt s)

-- |Applies 'addCondition' 'CondTemp' with the given temperature.
toTemp :: Int -> Recipe -> Recipe
toTemp t = addCondition (CondTemp t)

-- |Applies 'addCondition' 'CondTime' with the given time.
forTime :: Time -> Recipe -> Recipe
forTime t = addCondition (CondTime t)

-- |Creates an 'AND' condition.
(.&&) :: Condition -> Condition -> Condition
(.&&) = AND

-- |Creates an 'OR' condition.
(.||) :: Condition -> Condition -> Condition
(.||) = OR

-- |Creates a time with the given number of hours: 'hours' 1 = 'Time' 3600.
hours :: Time -> Time
hours = (*) 3600

-- |Creates a time with the given number of minutes: 'minutes' 1 = 'Time' 60.
minutes :: Time -> Time
minutes = (*) 60

-------------------------
-- Utility Functions
-------------------------

-- |Folds a function over a condition. AND will mappend the
-- values from folding over the two conditions whereas OR
-- takes the max.
foldCond :: (Ord a, Monoid a) => (Condition -> a) -> Condition -> a
foldCond f (c `AND` c') = (foldCond f c) `mappend` (foldCond f c')
foldCond f (c `OR` c')  = max (foldCond f c) (foldCond f c')
foldCond f c            = f c

-- |Fold a function over a recipe to obtain a value.
foldRecipe :: Monoid a => (Action -> a) -> Recipe -> a
foldRecipe f (Node a ts) =
    let vs = map (foldRecipe f) ts
     in f a `mappend` (mconcat vs)

-- |The ingredients of a recipe.
ingredients :: Recipe -> [String]
ingredients = foldRecipe f
    where
        f (GetIngredient s) = [s]
        f _                 = []

-- |List of ingredients paired with their measurements.
-- If no measurement is present for an ingredient, it is paired with "'Count' 0".
ingredientsQ :: Recipe -> [(String, Measurement)]
ingredientsQ (Node (Measure m) ts) = case ts of
    [Node (GetIngredient s) _] -> [(s,m)]
    _                          -> concatMap ingredientsQ ts
ingredientsQ (Node (GetIngredient s) _) = [(s, Count 0)]
ingredientsQ (Node _ ts) = concatMap ingredientsQ ts

type Label = Int

-- |Replaces the action in each node with a unique label.
labelRecipe :: Recipe -> Tree Label
labelRecipe r = fmap fst (labelRecipeR r)

-- |Labels each node of the tree while keeping the entire
-- recipe from that node downwards in a tuple.
labelRecipeR :: Recipe -> Tree (Label, Recipe)
labelRecipeR r = evalState (labelRecipeR' r) 1
    where
        labelRecipeR' r@(Node a ts) = do
            ts' <- mapM labelRecipeR' ts
            l <- get
            put (l + 1)
            return $ Node (l,r) ts'

-- |Same as 'labelRecipeR' but only keeps the action
-- from that node.
labelRecipeA :: Recipe -> Tree (Label, Action)
labelRecipeA r = fmap (\(l,r) -> (l, rootLabel r))
    (labelRecipeR r)

-- |Time to reach a certain temperature, for use with 'CondTemp'.
tempToTime :: Int -> Time
tempToTime i = Time i * 2

-- |Time taken to preheat to a given temperature, for use with 'heatAt'.
preheatTime :: Int -> Time
preheatTime = const $ Time 600

-- |Estimate of the time taken to execute a recipe using 'timeAction'.
time :: Recipe -> Time
time = foldRecipe timeAction

-- |Estimate of the time taken to perform a certain action.
timeAction :: Action -> Time
timeAction (GetIngredient _) = 10
timeAction Heat = 0
timeAction (HeatAt t) = preheatTime t
timeAction Wait = 0
timeAction (Combine _) = 10
timeAction (Conditional a c) = t' + foldCond f c
    where
        t' = timeAction a
        f (CondTime t) = t
        f (CondTemp t) = tempToTime t
        f (CondOpt s)  = 0
timeAction (Transaction a) = timeAction a
timeAction (Measure m) = 10

-- |Generates a list pairing each 'Action' in a 'Recipe'
-- with its dependencies i.e. the list of 'Action's within
-- its immediate child nodes.
deps :: Recipe -> [(Action, [Action])]
deps (Node a []) = [(a,[])]
deps (Node a ts) =
    let as = sort [x | (Node x _) <- ts]
     in sort $ (a,as) : concatMap deps ts

-- |Returns a list of all the possible topological sorts of a recipe.
topologicals :: Recipe -> [[Action]]
topologicals r = topologicals' (labelRecipeA r)
    where
        topologicals' :: Tree (Label, Action) -> [[Action]]
        topologicals' (Node (l,a) []) = [[a]]
        topologicals' t = concat
            [map (a:) (topologicals' $ removeFrom t lb) | (lb,a) <- leaves t]

-- |Returns True if the given node is a leaf i.e. has no child nodes.
isLeaf :: Tree (Label, Action) -> Bool
isLeaf (Node _ []) = True
isLeaf _           = False

-- |List of all leaves in the recipe.
leaves :: Tree a -> [a]
leaves (Node a []) = [a]
leaves (Node a ts) = concatMap leaves ts

-- |Removes all occurences of a label from the given tree.
-- Removing the label of the root node does nothing.
removeFrom :: Tree (Label, Action) -> Label -> Tree (Label, Action)
removeFrom (Node (l,a) ts) toRem = Node (l,a) ts''
    where
        ts' = filter (\(Node (l,a) ts) -> not $ l == toRem) ts
        ts'' = map (\t -> removeFrom t toRem) ts'
