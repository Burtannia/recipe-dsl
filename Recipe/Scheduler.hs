module Recipe.Scheduler where

import           Control.Monad.Trans.State
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Recipe             hiding (removeFrom, deleteAll, leaves)
import Data.List (groupBy, sortBy)

-----------------------------
-- Label Helper Functions
-----------------------------

-- map of labels to their recipe
mkLabelMap :: Tree (Label, Recipe) -> Map Label Recipe
mkLabelMap = Map.fromList . flatten

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

type Schedule = Map StName Stack

-- heuristic 1 (least demand):

duration :: Label -> Map Label Recipe -> Time
duration l rMap = case Map.lookup l rMap of
    Nothing -> error "Recipe not found"
    Just (Node a ts) -> timeAction a

removeFrom :: Tree Label -> Label -> Tree Label
removeFrom t@(Node a ts) toRem = Node a ts''
    where
        ts'  = deleteAll toRem ts
        ts'' = map (\t -> removeFrom t toRem) ts'

deleteAll :: Label -> [Tree Label] -> [Tree Label]
deleteAll _ [] = []
deleteAll l (y@(Node l' _):ys)
    | l == l' = deleteAll l ys
    | otherwise = y : deleteAll l ys

demands :: Tree Label -> Label -> Env -> Map Label Recipe -> [(StName, Time)]
demands tree leaf env rMap =
    let unscheduleds = removeFrom tree leaf
        durations = concat . flatten $ fmap expectedDurs unscheduleds
        expectedDurs = \l -> [(v, Time $ dur `div` length vs) | let vs = lookupValSts env l rMap
                                                              , v <- vs
                                                              , let (Time dur) = duration l rMap]
        sortedDurs = sortBy (\x y -> compare (fst x) (fst y)) durations
        groupedDurations = groupBy (\x y -> fst x == fst y) sortedDurs
        foldF = \(st,t) (_,t') -> (st, t + t')
        inDemands = map (\ds -> foldr1 foldF ds) groupedDurations
        inDemNames = map fst inDemands
        allSts = map stName (eStations env)
        unuseds = filter (\st -> st `notElem` inDemNames) allSts
     in inDemands ++ map (\st -> (st, Time 0)) unuseds
    -- remove leaf from recipe
    -- iterate over tree constructing [(StName, Duration `div` num valid stations)]
    -- group by stname and collapse into 1 tuple for each stname
    -- need to add in (name,0) for stations that didn't appear

-- heauristic 2 (least idle required):

-- |Given a label and a list of valid stations with their stacks, returns
-- a list of those stations with the required idle time to schedule that label.
-- Must also be passed a list of all stacks in order to find dependency times.
-- The tree passed must be the full tree of the labelled recipe without
-- already scheduled leaves having been removed.
idleTime :: Label -> [(StName, Stack)] -> [Stack] -> Tree Label -> Map Label Recipe -> [(StName, Time)]
idleTime l validSts allStacks fullTree rMap =
    let deps = childLabels l fullTree
        ends = map (\d -> endOfLabel d allStacks rMap) deps -- :: [Time]
        minStart = maximum ends
        idles = map (\(n,s) -> (n, minStart - stackHeight s rMap)) validSts
     in if length deps == 0
            then map (\(s,_) -> (s, Time 0)) validSts
            else map (\(n,t) -> if t < Time 0
                                    then (n, Time 0)
                                    else (n,t)) idles

-- get end of time label in stacks
endOfLabel :: Label -> [Stack] -> Map Label Recipe -> Time
endOfLabel _ [] _ = error "no stacks"
endOfLabel l ss rMap = maximum $ map endOfLabel' ss
    where
        endOfLabel' stack =
            let stack' = dropWhile (\x -> not $ x == Active l) stack
             in sumDurations stack' rMap


sumDurations :: [Task] -> Map Label Recipe -> Time
sumDurations [] _ = Time 0
sumDurations (Active l : ts) rMap = duration l rMap + sumDurations ts rMap
sumDurations (Idle t : ts) rMap = t + sumDurations ts rMap

-- heuristic 3 (most space):

mostSpace :: [(StName, Stack)] -> Map Label Recipe -> (StName, Stack)
mostSpace [] _ = error "no stacks"
mostSpace [x] _ = x
mostSpace (x@(stName, stack) : y@(stName', stack') : xs) rMap
    | stackHeight stack rMap > stackHeight stack' rMap = mostSpace (y:xs) rMap
    | otherwise = mostSpace (x:xs) rMap

stackHeight :: Stack -> Map Label Recipe -> Time
stackHeight = sumDurations

isTransaction :: Label -> Map Label Recipe -> Bool
isTransaction l rMap = case Map.lookup l rMap of
    Just (Node (Transaction _) _) -> True
    _ -> False

-- need to move this over to all Maps not []
-- heuristic 1 - heuristic 2
-- tie breaker = heuristic 3
chooseStack :: Tree Label -> Tree Label -> Label -> Env -> Map Label Recipe -> Schedule -> Schedule
chooseStack fullTree lTree l env rMap sch =
    let vs = lookupValSts env l rMap
        validSch = Map.filterWithKey (\st _ -> st `elem` vs) sch -- schedule containing only stations valid for l

        ds = demands lTree l env rMap -- :: [(StName, Time)]
        ds' = filter (\(st,_) -> st `elem` Map.keys validSch) ds
        is = idleTime l (Map.toList validSch) (Map.elems sch) fullTree rMap -- :: [(StName, Time)]
        dMinusIs = map (\(st,dur) ->
            (st, dur - (fromJust $ lookup st is))) ds' -- need to correct negatives to 0

        sts = sortBy (\(_,t) (_,t') -> compare t t') dMinusIs
        (st,min) = head sts
        mins = filter (\(st,t) -> t == min) sts
        minNames = map fst mins

        (bestName, bestStack) = if length mins == 1
            then let bestName = head minNames
                  in (bestName, fromJust $ Map.lookup bestName sch)
            else mostSpace (Map.toList $
                Map.filterWithKey (\k v -> k `elem` minNames) sch) rMap

        iTime = fromJust $ lookup bestName is
        newStack = case iTime of
            Time 0 -> Active l : bestStack
            _ -> Active l : Idle iTime : bestStack
     in Map.insert bestName newStack sch

scheduleRecipe :: Recipe -> Env -> Schedule
scheduleRecipe r env = 
    let lTree = labelRecipe r
        rMap = mkLabelMap $ labelRecipeR r
     in scheduleRecipe' lTree lTree rMap (initSchedule env)
    where
        scheduleRecipe' :: Tree Label -> Tree Label -> Map Label Recipe -> Schedule -> Schedule
        scheduleRecipe' fullTree lTree rMap sch =
            let ls = leaves lTree rMap
                shortL = shortest ls rMap
             in if isTransaction shortL rMap
                    then initSchedule env
                    else
                        let schWithLeaf = chooseStack fullTree lTree shortL env rMap sch
                         in if shortL == rootLabel lTree
                                then schWithLeaf
                                else scheduleRecipe' fullTree (removeFrom lTree shortL) rMap schWithLeaf

-- sch1 values kept on collision as per Map.insert
mergeInto :: Schedule -> Schedule -> Schedule
mergeInto sch1 sch2 = recInsert (Map.toList sch1) sch2
    where
        recInsert [] sch = sch
        recInsert ((k,v):xs) sch =
            let sch' = Map.insert k v sch
             in recInsert xs sch'

-- if transaction (need to check if node above leaf is a transaction):
-- get list of valid stations / stacks for each part of transaction
-- aim to schedule children to finish at same time
-- schedule parent
-- remember when calculating "unscheduleds" to pass parent so entire transaction is removed

-- else:
-- get list of valid stations / stacks
-- choose stack using heuristics
-- push onto stack

initSchedule :: Env -> Schedule
initSchedule env = Map.fromList
    [(st,[]) | st <- map stName (eStations env)]

shortest :: [Label] -> Map Label Recipe -> Label
shortest [] _ = error "No leaves"
shortest [l] _ = l
shortest (l:x:ls) rMap
    | duration l rMap > duration x rMap = shortest (x:ls) rMap
    | otherwise = shortest (l:ls) rMap

leaves :: Tree Label -> Map Label Recipe -> [Label]
leaves (Node l []) _ = [l]
leaves (Node l ts) rMap = case Map.lookup l rMap of
    Just (Node (Transaction _) rs) ->
        if map subForest rs == []
            then [l]
            else concatMap (\t -> leaves t rMap) ts
    _ -> concatMap (\t -> leaves t rMap) ts