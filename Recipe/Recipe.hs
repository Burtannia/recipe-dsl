module Recipe.Recipe where

import Data.List
import Data.Maybe (listToMaybe, fromJust, isJust, catMaybes)
import Data.Tree
import Control.Applicative
import Control.Monad.State

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

data Recipe = Ingredient String
            | HeatAt Temperature Recipe
            | Wait Time Recipe
            | Combine Recipe Recipe
            | Conditional Condition Recipe
            | Transaction Recipe
            | Measure Measurement Recipe
            deriving (Show, Eq)

type Measurement = Int
type Time = Int

data Condition = CondTime Time | CondTemp Temperature | CondOpt
    | Condition `AND` Condition | Condition `OR` Condition
    deriving (Show, Eq)

data Temperature = Deg Int | Low | Medium | High
    deriving (Show, Eq)

heatAt :: Temperature -> Recipe -> Recipe
heatAt = HeatAt

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

wait :: Time -> Recipe -> Recipe
--wait t (Wait t' r) = wait (t + t') r
wait t r = Wait t r

conditional :: Condition -> Recipe -> Recipe
conditional = Conditional

transaction :: Recipe -> Recipe
--transaction r@(Transaction _) = r
transaction = Transaction

measure :: Measurement -> Recipe -> Recipe
measure = Measure

-------------------------------------
-- RECIPE LABELLING
-------------------------------------

-- abstract this into mapping of Recipes to Properties

type Label = Int

createTable :: Recipe -> [(Label, Recipe)]
createTable r = zip [1..length ps] ps
    where
        ps = rmdups $ parts r

-- Unsafe
getLabel :: [(Label, Recipe)] -> Recipe -> Label
getLabel lrs r = fst $ head lrs'
    where lrs' = filter (\x -> snd x == r) lrs

-- Unsafe
getRecipe :: [(Label, Recipe)] -> Label -> Recipe
getRecipe lrs l = snd $ head lrs'
    where lrs' = filter (\x -> fst x == l) lrs

recipeToTree :: (Recipe -> a) -> Recipe -> Tree a
recipeToTree f r = Node (f r) cs'
    where
        cs = childRecipes r
        cs' = map (recipeToTree f) cs

labelRecipe :: Recipe -> Tree Label
labelRecipe r = recipeToTree (getLabel table) r
    where table = createTable r

-- should process recipe e.g. apply transactions
-- etc. before top sorting

topologicals :: Eq a => Tree a -> [[a]]
topologicals (Node a [])  = [[a]]
topologicals t = concat
    [map (a:) (topologicals' l) | l@(Node a _) <- ls]
    where
        topologicals' l = topologicals $ removeFrom t l
        ls = leaves t

isLeaf :: Tree a -> Bool
isLeaf (Node _ []) = True
isLeaf _ = False

leaves :: Tree a -> [Tree a]
leaves (Node a []) = [Node a []]
leaves (Node a ts) = concatMap leaves ts

-- Removes all occurences of a sub tree from the given tree.
-- Removing a tree from itself does nothing.
removeFrom :: Eq a => Tree a -> Tree a -> Tree a
removeFrom t@(Node a ts) toRem = Node a ts''
    where
        ts'  = deleteAll toRem ts
        ts'' = map (\t -> removeFrom t toRem) ts'

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll x (y:ys)
    | x == y = deleteAll x ys
    | otherwise = y : deleteAll x ys

-------------------------------------
-- RECIPE SEMANTICS
-------------------------------------

data Action =
    Input -- input and output take Recipe?
    | Output
    | Preheat Temperature
    | DoNothing Time
    | Mix Recipe Recipe
    | EvalCond Condition
    | MeasureOut Measurement Recipe
    | Hold Recipe -- tap "Holds" water
    deriving Show

-------------------------------------
-- CONCRETE IMPLEMENTATION
-------------------------------------

data Env = Env
    { eStations :: [Station]
    , eEntries  :: [(Recipe, StName)] -- where things start
    }

type StName = String

data Obs = ObsTemp Temperature

-- Currently assumed that all stations are
-- accessible in some way by a transfer node e.g. human
data Station = Station
    { stName     :: String
    , stInputs   :: [StName] -- List of names of stations
    , stOutputs  :: [StName] -- Better to name Connections String In|Out ?
    , stConstrF  :: ConstraintF
    , stTransfer :: Bool -- Is transfer node? could end up being Maybe f where f is how to transfer
    , stObs      :: [IO Obs]
    }

-- match a recipe against constraint function
-- returns a list of actions for the recipe if possible
type ConstraintF = Recipe -> Maybe [Action]

-- bit of an issue with CondOpt as needs to be added
-- before action i.e. after Input
addEvalCond :: Condition -> [Action] -> Maybe [Action]
addEvalCond c as = return $ init as ++ [EvalCond c, Output]

type Schedule = [(StName, [Action])]

entryPoint :: Env -> Recipe -> Maybe StName
entryPoint env r = listToMaybe $ findRecipe env r

-- Check if the recipe is stored somewhere
-- in the environment already
findRecipe :: Env -> Recipe -> [StName]
findRecipe env r = [snd x | x <- eEntries env,
                            fst x == r]

makeSchedule :: Env -> Recipe -> Maybe Schedule
makeSchedule env r = case r of
    (Ingredient _)     -> makeSchedule' (const Nothing)
    (HeatAt _ r')      -> makeSchedule' (singleChild r')
    (Combine r1 r2)    -> makeSchedule' (doubleChild r1 r2)
    (Wait _ r')        -> makeSchedule' (singleChild r')
    (Conditional _ r') -> makeSchedule' (singleChild r')
    (Transaction r')   -> makeSchedule' (singleChild r') -- does this matter atm as we are doing things in order anyway?
    (Measure _ r')     -> makeSchedule' (singleChild r')
    where
        makeSchedule' scheduleCons = case entryPoint env r of
            Nothing -> assignStation env r >>= scheduleCons
            Just s  -> Just [(s, [Hold r])]
        singleChild r' sa = (++) <$> makeSchedule env r' <*> Just [sa]
        doubleChild r1 r2 sa = (++) <$> ((++)
            <$> makeSchedule env r1 <*>
            makeSchedule env r2) <*> Just [sa]

assignStation :: Env -> Recipe -> Maybe (StName, [Action])
assignStation env r = listToMaybe
    [(stName st, fromJust ma) | st <- eStations env,
                                let ma = (stConstrF st) r,
                                isJust ma]

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

mapRecipe :: (Recipe -> a) -> Recipe -> [a]
mapRecipe f r = f r : concatMap (mapRecipe f) cs
    where cs = childRecipes r

mapRecipe' :: (Recipe -> Maybe a) -> Recipe -> [a]
mapRecipe' f r = catMaybes $ mapRecipe f r

childRecipes :: Recipe -> [Recipe]
childRecipes r = case r of
    Ingredient s     -> []
    HeatAt _ r'      -> [r']
    Wait _ r'        -> [r']
    Combine r1 r2    -> [r1,r2]
    Conditional _ r' -> [r']
    Transaction r'   -> [r']
    Measure _ r'     -> [r']

parts :: Recipe -> [Recipe]
parts = mapRecipe id

rmdups :: Eq a => [a] -> [a]
rmdups xs = rmdups' xs []
    where
        rmdups' [] ys = ys
        rmdups' (x:xs) ys = if x `elem` ys
            then rmdups' xs ys
            else rmdups' xs (ys ++ [x])

ppList :: Show a => [a] -> IO ()
ppList [] = return ()
ppList (x:xs) = print x
    >> putStrLn ""
    >> ppList xs

-- Create a list of ingredients used in a recipe
-- Doesn't yet show quantities
ingredients :: Recipe -> [String]
ingredients (Ingredient s) = [s]
ingredients r =
    concatMap ingredients (childRecipes r)