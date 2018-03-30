module Recipe.Scheduler where

import           Control.Monad.Trans.State
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Recipe             hiding (Label, removeFrom, deleteAll, leaves)
import Data.List (groupBy, sortBy)

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
        groupedDurations = groupBy (\x y -> fst x == fst y) durations
        foldF = \(st,t) (_,t') -> (st, t + t')
     in map (\ds -> foldr1 foldF ds) groupedDurations
    -- remove leaf from recipe
    -- iterate over tree constructing [(StName, Duration `div` num valid stations)]
    -- group by stname and collapse into 1 tuple for each stname

-- heauristic 2 (least idle required):

-- |Given a label and a list of valid stations with their stacks, returns
-- a list of those stations with the required idle time to schedule that label.
idleTime :: Label -> [(StName, Stack)] -> Tree Label -> Map Label Recipe -> [(StName, Time)]
idleTime l ss lTree rMap =
    let deps = childLabels l lTree
        stacks = map snd ss
        ends = map (\d -> endOfLabel d stacks rMap) deps -- :: [Time]
        minStart = maximum ends
        idles = map (\(n,s) -> (n, minStart - stackHeight s rMap)) ss
     in if length deps == 0
            then map (\(s,_) -> (s, Time 0)) ss
            else map (\(n,t) -> if t < Time 0
                                    then (n, Time 0)
                                    else (n,t)) idles

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
chooseStack :: Tree Label -> Label -> Env -> Map Label Recipe -> Schedule -> Schedule
chooseStack lTree l env rMap sch =
    let ds = demands lTree l env rMap -- :: [(StName, Time)]
        ds' = filter (\(st,_) -> st `elem` Map.keys sch) ds
        is = idleTime l (Map.toList sch) lTree rMap -- :: [(StName, Time)]
        dMinusIs = map (\(st,dur) ->
            (st, dur - (fromJust $ lookup st is))) ds'
        sts@((st,min):_) = sortBy (\(_,t) (_,t') -> compare t t') dMinusIs
        mins = filter (\(st,t) -> t == min) sts
        minNames = map fst mins
        (bestName, bestStack) = if length mins == 1
            then let bestName = head minNames
                  in (bestName, fromJust $ Map.lookup bestName sch)
            else mostSpace (Map.toList $
                Map.filterWithKey (\k v -> k `elem` minNames) sch) rMap
        iTime = fromJust $ lookup bestName is
     in Map.insert bestName (Active l : Idle iTime : bestStack) sch


-- scheduleRecipe :: Recipe -> Env -> Schedule
-- scheduleRecipe r env = 
--     let lTree = mkLabelTree r
--         rMap = mkLabelMap $ mkLabelTreeR r
--      in scheduleRecipe' lTree rMap (initSchedule env)
--     where
--         scheduleReciple' lTree rMap sch =
--             let ls = leaves lrTree
--                 shortL = shortest ls
--              in if isTransaction shortL
--                     then
--                     else
--                         let vs = validStations env shortL
--                             validSch = Map.filterWithKey (\st -> st `elem` vs) sch
--                             schWithLeaf = chooseStack lTree leaf env rMap validSch shortL
                            -- choose stack takes value and adds to chosen stack returning updated schedule
                            -- merge that updated schedule into original

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

leaves :: Tree (Label, Recipe) -> [Label]
leaves (Node (l,_) []) = [l]
leaves (Node (l, Node (Transaction a) rs) ts) =
    if map subForest rs == []
        then [l]
        else concatMap leaves ts
leaves (Node _ ts) = concatMap leaves ts