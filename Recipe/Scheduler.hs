module Recipe.Scheduler where

import           Control.Monad.Trans.State
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Recipe             hiding (Label, removeFrom, deleteAll)
import Data.List (groupBy)

-----------------------------
-- Label Recipe
-----------------------------

type Label = String

-- labels recipe tree with action1, action2...
mkLabelTreeR :: Recipe -> Tree (Label, Recipe)
mkLabelTreeR r = fmap (\(i,r) -> ("action" ++ show i, r)) lr
    where lr = labelRecipeR r

-- mkLabelTreeR without keeping the recipe
mkLabelTree :: Recipe -> Tree Label
mkLabelTree r = fmap (\(l,_) -> l) (mkLabelTreeR r)

-- map of labels to their recipe
mkLabelMap :: Tree (Label, Recipe) -> Map Label Recipe
mkLabelMap = Map.fromList . flatten

-----------------------------
-- Label Helper Functions
-----------------------------

-- Label -> Recipe
lookupR :: Label -> Map Label Recipe -> Recipe
lookupR l rMap = fromJust $ Map.lookup l rMap

-- Label -> [ChildLabels]
childLabels :: Label -> Tree Label -> [Label]
childLabels l (Node l' ts)
    | l == l'   = map rootLabel ts
    | otherwise = concatMap (childLabels l) ts

-- Env -> Label -> [Valid Stations]
lookupValSts :: Env -> Label -> Map Label Recipe -> [StName]
lookupValSts env l rMap = let r = lookupR l rMap
                           in validStations env r

-- Env -> Recipe -> [Valid Stations]
validStations :: Env -> Recipe -> [StName]
validStations env r = let xs = map (applyConstrF r) (eStations env)
                          applyConstrF r st = (stName st, (stConstrF st) r)
                          ys = filter (isJust . snd) xs
                       in map fst ys

-----------------------------
-- Scheduler
-----------------------------

data Task = Active Label | Idle Time
    deriving (Eq, Show)

type Stack = [Task]

data Schedule = Map StName Stack

-- heuristic 1 (least demand):
-- given a tree, we select a leaf
-- remaining "unscheduled" actions are that tree - the leaf

duration :: Label -> Map Label Recipe -> Time
duration l rMap = case Map.lookup l rMap of
    Nothing -> Time 0
    Just (Node a ts) -> timeAction a

removeFrom :: Tree Label -> Tree Label -> Tree Label
removeFrom t@(Node a ts) toRem = Node a ts''
    where
        ts'  = deleteAll toRem ts
        ts'' = map (\t -> removeFrom t toRem) ts'

deleteAll :: Tree Label -> [Tree Label] -> [Tree Label]
deleteAll _ [] = []
deleteAll x (y:ys)
    | x == y = deleteAll x ys
    | otherwise = y : deleteAll x ys

demands :: Tree Label -> Tree Label -> Env -> Map Label Recipe -> [(StName, Time)]
demands tree leaf env rMap =
    let unscheduleds = removeFrom tree leaf
        durations = concat . flatten $ fmap expectedDurs unscheduleds
        expectedDurs = \l -> [(v, Time $ dur `div` length vs) | let vs = lookupValSts env l rMap
                                                              , v <- vs
                                                              , let (Time dur) = duration l rMap]
        groupedDurations = groupBy (\x y -> fst x == fst y) durations
        foldF = \(st,t) (_,t') -> (st, t + t')
     in map (\ds -> foldr1 foldF ds) groupedDurations
    -- remove leaf from recipe
    -- iterate over tree constructing [(StName, Duration `div` num valid stations)]
    -- group by stname and collapse into 1 tuple for each stname

-- heauristic 2 (least idle required):

-- get end of time label in stacks
endOfLabel :: Label -> [Stack] -> Map Label Recipe -> Time
endOfLabel _ [] _ = Time 0
endOfLabel l ss rMap = maximum $ map endOfLabel' ss
    where
        endOfLabel' [] = Time 0
        endOfLabel' (t:ts)
            | t == Active l = sumDurations (t:ts) rMap
            | otherwise = endOfLabel' ts

sumDurations :: [Task] -> Map Label Recipe -> Time
sumDurations [] _ = Time 0
sumDurations (Active l : ts) rMap = duration l rMap + sumDurations ts rMap
sumDurations (Idle t : ts) rMap = t + sumDurations ts rMap


--idleTime :: Label -> [Stack] -> [(StName, Int)]
-- get dependencies
-- get endOfLabel dependencies
-- take highest
-- for each stack get the highest dep - height of stack
-- that is the req idle time (0 if negative)

-- heuristic 3 (most space):
mostSpace :: [Stack] -> Stack
mostSpace [] = error "no stacks"
mostSpace [x] = x
mostSpace (x:y:xs)
    | stackHeight x > stackHeight y = mostSpace (y:xs)
    | otherwise = mostSpace (x:xs)

stackHeight :: Stack -> Int
stackHeight s = 0

-- heuristic 1 - heuristic 2
-- tie breaker = heuristic 3
chooseStack :: [Stack] -> Stack
chooseStack = undefined