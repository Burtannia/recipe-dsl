{-|The scheduling function implemented here is mainly for demonstration purposes.
   It is rather inefficient and runs for a very long time thus it is not suitable
   for scheduling recipes with more than a small number of 'Action's.

   == Leaf Selection Heuristics
   1. Shortest task.
   2. Longest branch.

   == Stack Selection Heuristics
   1. Least in-demand first. Calculates an expected time that will be added
   to the stack by all the unscheduled tasks.
   2. Least idle time required i.e. the stack which has a height closest
   to the minimum start time of the action being scheduled.
   3. Smallest stack.
-}

module Recipe.Scheduler(
    -- * Types
    Task (..), Stack, Schedule,
    -- * Label Helper Functions
    mkLabelMap, lookupR, childLabels,
    lookupValSts, validStations,
    -- * Scheduling Functions
    duration, scheduleRecipe, initSchedule,
    schLength,
    applyOpts ) where

import           Data.List       (groupBy, maximumBy, minimumBy, sortBy, intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isJust, catMaybes)
import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Recipe   hiding (leaves, removeFrom)

-----------------------------
-- Label Helper Functions
-----------------------------

-- |Create a map of labels to their recipe in the
-- given tree.
mkLabelMap :: Tree (Label, Recipe) -> Map Label Recipe
mkLabelMap = Map.fromList . flatten

-- |Given a label, lookup the corresponding recipe
-- in the map.
lookupR :: Label -> Map Label Recipe -> Recipe
lookupR l rMap = fromJust $ Map.lookup l rMap

-- |Get the child labels of a given label in
-- a given tree.
childLabels :: Label -> Tree Label -> [Label]
childLabels l (Node l' ts)
    | l == l'   = map rootLabel ts
    | otherwise = concatMap (childLabels l) ts

-- |Lookup the stations capable of handling the action
-- at the root node of the recipe corresponding the to given label.
lookupValSts :: Env -> Label -> Map Label Recipe -> [StName]
lookupValSts env l rMap =
    let r@(Node a _) = lookupR l rMap
     in case getUsings a of
            Just sts -> sts
            Nothing -> validStations env r

-- |Lookup the stations capable of handling the action
-- at the root node of the given recipe.
validStations :: Env -> Recipe -> [StName]
validStations env r@(Node a _) =
    case getUsings a of
        Just sts -> sts
        Nothing -> 
            let xs = map (applyConstrF r) (eStations env)
                applyConstrF r st = (stName st, (stConstrF st) r)
                ys = filter (isJust . snd) xs
             in map fst ys

-----------------------------
-- Scheduler
-----------------------------

findStByName :: StName -> [Station] -> Maybe Station
findStByName stNm [] = Nothing
findStByName stNm (st:sts)
    | stNm == stName st = Just st
    | otherwise = findStByName stNm sts

parseUsings :: Recipe -> [Station] -> Maybe String
parseUsings r sts = parseUsings' r
    where
        parseUsings' (Node (Using a stNs) ts) =
            let ms = map (\stNm -> findStByName stNm sts) stNs
            in if hasNothing ms then
                    Just $
                    "Invalid \"Using\" Constraint:\n"
                    ++ show (Using a stNs)
                else
                    parseSubTrees ts
        parseUsings' (Node a ts) =
            let a' = popWrapperA a
             in if a == a' then
                    parseSubTrees ts
                else
                    parseUsings (Node a' ts) sts
        parseSubTrees ts = mconcatErrors $
            map (\t -> parseUsings t sts) ts

mconcatErrors :: [Maybe String] -> Maybe String
mconcatErrors mes =
    if all (== Nothing) mes then
        Nothing
    else
        Just $ concat
        $ intersperse "\n"
        $ catMaybes mes
                
hasNothing :: [Maybe a] -> Bool
hasNothing [] = False
hasNothing (Nothing:xs) = True
hasNothing (_:xs) = hasNothing xs

applyOptsA :: Action -> [Option] -> Maybe Action
applyOptsA (Optional s a) opts
    | evalOpt s opts = applyOptsA a opts
    | otherwise = Nothing
applyOptsA (Transaction a) opts =
    applyOptsA a opts >>= return . Transaction
applyOptsA (Conditional a c) opts =
    applyOptsA a opts >>= return . \a' -> Conditional a' c
applyOptsA (Using a sts) opts =
    applyOptsA a opts >>= return . \a' -> Using a' sts
applyOptsA a _ = Just a

applyOpts :: Recipe -> [Option] -> Maybe Recipe
applyOpts (Node a ts) opts = case applyOptsA a opts of
    Nothing -> case ts of
        [] -> Nothing
        [x] -> applyOpts x opts
        xs -> maxByLength xs >>= \r -> applyOpts r opts
    Just a' ->
        let ts' = catMaybes $ map (\t -> applyOpts t opts) ts
         in if length ts' == reqDeps a' then
                Just $ Node a' ts'
            else
                Nothing
       
reqDeps :: Action -> Int
reqDeps (Combine _) = 2
reqDeps (GetIngredient _) = 0
reqDeps a
    | popWrapperA a == a = 1
    | otherwise = reqDeps $ popWrapperA a

maxByLength :: [Tree a] -> Maybe (Tree a)
maxByLength [] = Nothing
maxByLength [t] = Just t
maxByLength ts = Just $ maximumBy
    (\t1 t2 -> compare (length t1) (length t2)) ts

-- |Task performed by a 'Station'.
data Task a = Active a -- ^ Active performing the action corresponding to the label.
            | Idle Time -- ^ Idle for a time.
    deriving (Eq, Show)

type Stack a = [Task a]

-- |Schedule is a map of station names to their 'Stack' of 'Task's
type Schedule a = Map StName (Stack a)

-- |Time a schedule takes, must be given the recipe that
-- the schedule is for.
schLength :: Schedule Label -> Recipe -> Time
schLength sch r =
    let rMap = mkLabelMap $ labelRecipeR r
        stacks = Map.elems sch
        ts = map (\s -> stackHeight s rMap) stacks
     in maximum ts

-- heuristic 1 (least demand):

-- |Duration that the root action of the recipe
-- corresponding to the given label will take.
-- Uses 'timeAction'.
duration :: Label -> Map Label Recipe -> Time
duration l rMap = case Map.lookup l rMap of
    Nothing          -> error "Recipe not found"
    Just (Node a ts) -> timeAction a

-- Removes all instances of a label from a tree
-- of labels. If the label is in the root node,
-- that node is not removed.
removeFrom :: Tree Label -> Label -> Tree Label
removeFrom t@(Node a ts) toRem = Node a ts''
    where
        ts'  = deleteAll toRem ts
        ts'' = map (\t -> removeFrom t toRem) ts'

-- Deletes all trees in a list whose root node contains
-- the given label.
deleteAll :: Label -> [Tree Label] -> [Tree Label]
deleteAll _ [] = []
deleteAll l (y@(Node l' _):ys)
    | l == l' = deleteAll l ys
    | otherwise = y : deleteAll l ys

-- Calculates demand for each station:
-- remove leaf from recipe
-- iterate over tree constructing [(StName, Duration `div` num valid stations)] for each label
-- group by stname and collapse into 1 tuple for each stname
-- add in (name,0) for stations that didn't appear
demands :: Tree Label -> Env -> Map Label Recipe -> [(StName, Time)]
demands unscheduleds env rMap =
    let durations = concat . flatten $ fmap expectedDurs unscheduleds
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

-- heauristic 2 (least idle required):

-- Given a label and a list of valid stations with their stacks, returns
-- a list of those stations with the required idle time to schedule that label.
-- Must also be passed a list of all stacks in order to find dependency times.
-- The tree passed must be the full tree of the labelled recipe without
-- already scheduled leaves having been removed.
idleTime :: Label -> [(StName, Stack Label)] -> [Stack Label] -> Tree Label -> Map Label Recipe -> [(StName, Time)]
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
endOfLabel :: Label -> [Stack Label] -> Map Label Recipe -> Time
endOfLabel _ [] _ = error "no stacks"
endOfLabel l ss rMap = maximum $ map endOfLabel' ss
    where
        endOfLabel' stack =
            let stack' = dropWhile (\x -> not $ x == Active l) stack
             in sumDurations stack' rMap

-- Sum the durations of a list of stacks.
sumDurations :: [Task Label] -> Map Label Recipe -> Time
sumDurations [] _                 = Time 0
sumDurations (Active l : ts) rMap = duration l rMap + sumDurations ts rMap
sumDurations (Idle t : ts) rMap   = t + sumDurations ts rMap

-- heuristic 3 (most space):

-- Given a list of station names and their stacks, return the pair
-- with the most space.
mostSpace :: [(StName, Stack Label)] -> Map Label Recipe -> (StName, Stack Label)
mostSpace [] _ = error "no stacks"
mostSpace [x] _ = x
mostSpace (x@(stName, stack) : y@(stName', stack') : xs) rMap
    | stackHeight stack rMap > stackHeight stack' rMap = mostSpace (y:xs) rMap
    | otherwise = mostSpace (x:xs) rMap

stackHeight :: Stack Label -> Map Label Recipe -> Time
stackHeight = sumDurations

-- Is the root action of the recipe corresponding
-- to the given label wrapped in a transaction?
isTransaction :: Label -> Map Label Recipe -> Bool
isTransaction l rMap = case Map.lookup l rMap of
    Just (Node (Transaction _) _) -> True
    _                             -> False

getUsings :: Action -> Maybe [StName]
getUsings (Using _ sts) = Just sts
getUsings a
    | popWrapperA a == a = Nothing
    | otherwise = getUsings $ popWrapperA a

-- Choose the best stack for the given label.
-- heuristic 1 + heuristic 2
-- tie breaker is heuristic 3
-- passed fullTree and tree with label removed
chooseStack :: Tree Label -> Tree Label -> Label -> Env
    -> Map Label Recipe -> Schedule Label -> Schedule Label
chooseStack fullTree unscheduleds l env rMap sch =
    let vs = lookupValSts env l rMap
        validSch = Map.filterWithKey (\st _ -> st `elem` vs) sch -- schedule containing only stations valid for l

        ds = demands unscheduleds env rMap -- :: [(StName, Time)]
        ds' = filter (\(st,_) -> st `elem` Map.keys validSch) ds

        is = idleTime l (Map.toList validSch) (Map.elems sch) fullTree rMap -- :: [(StName, Time)]
        dPlusIs = map (\(st,demand) ->
            (st, demand + (fromJust $ lookup st is))) ds'

        sts = sortBy (\(_,t) (_,t') -> compare t t') dPlusIs
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
            _      -> Active l : Idle iTime : bestStack
     in Map.insert bestName newStack sch

-- Recursive version of chooseStack to work on a list of labels.
-- Used to schedule all children of a transaction without other recipes
-- being scheduled in between.
chooseStackRec :: Tree Label -> Tree Label -> [Label] -> Env
    -> Map Label Recipe -> Schedule Label -> Schedule Label
chooseStackRec fullTree lTree' [] env rMap sch = sch
chooseStackRec fullTree lTree' (l:ls) env rMap sch =
    let sch' = chooseStack fullTree lTree' l env rMap sch
     in chooseStackRec fullTree lTree' ls env rMap sch'

-- |Schedules a recipe in the given environment.
-- Will error if no schedule is available e.g. if there
-- is no 'Station' capable of handling a certain 'Action'.
-- See documentation at the top for heuristics used.
scheduleRecipe :: Recipe -> Env -> Schedule Label
scheduleRecipe r env =
    let mUseErr = parseUsings r (eStations env)
        mr = applyOpts r (eOpts env)
     in case mUseErr of
            Just er -> error er
            Nothing -> case mr of
                Nothing -> error "False option resulted in no recipe."
                Just r' -> 
                    let lTree = labelRecipe r'
                        rMap = mkLabelMap $ labelRecipeR r'
                     in scheduleRecipe' lTree lTree rMap (initSchedule env)
    where
        scheduleRecipe' :: Tree Label -> Tree Label
            -> Map Label Recipe -> Schedule Label -> Schedule Label
        scheduleRecipe' fullTree lTree rMap sch =
            let ls = leaves lTree rMap
                shortL = chooseLeaf ls fullTree rMap
                lTree' = removeFrom lTree shortL
                newSch = if isTransaction shortL rMap
                    then
                        let deps = childLabels shortL fullTree
                            sch' = chooseStackRec fullTree lTree' deps env rMap sch -- schedule child recipes of transaction
                            adjSch = adjustSch sch' deps rMap
                         in chooseStack fullTree lTree' shortL env rMap adjSch      -- schedule parent of transaction
                    else
                        chooseStack fullTree lTree' shortL env rMap sch
             in if shortL == rootLabel fullTree
                    then newSch
                    else scheduleRecipe' fullTree lTree' rMap newSch

-- Given a schedule containing the labels passed as the second
-- argument, add idle time before them so that they all finish
-- at the same time.
adjustSch :: Schedule Label -> [Label] -> Map Label Recipe -> Schedule Label
adjustSch sch ls rMap =
    let stacks = Map.elems sch
        ends = map (\l -> (l, endOfLabel l stacks rMap)) ls
        (_,latest) = maximumBy (\(_,t) (_,t') -> compare t t') ends
        notLatests = filter (\(_,t) -> not $ t == latest) ends
        idleReqs = map (\(l,t) -> (l, latest - t)) notLatests
     in adjustStacks sch idleReqs

-- Helper function used by 'adjustSch', applied the idle
-- time paired with each label by adding it before
-- any occurences of that label in the stacks.
adjustStacks :: Schedule Label -> [(Label, Time)] -> Schedule Label
adjustStacks sch [] = sch
adjustStacks sch ((l,t):ls) =
    let filteredSch = Map.filter (\s -> Active l `elem` s) sch
        (name,stack) = head $ Map.toList filteredSch
        stack' = addIdleTime l t stack
        sch' = Map.insert name stack sch
     in adjustStacks sch' ls

-- Add the given idle time after the given label
-- in the stack.
addIdleTime :: Label -> Time -> Stack Label -> Stack Label
addIdleTime l t stack =
    let (xs,ys) = splitAtEq (Active l) stack
        ys' = case ys of
                Active l : Idle t' : zs -> Active l : Idle (t + t') : zs
                Active l : zs           -> Active l : Idle t : zs
                _                       -> ys
     in xs ++ ys'

-- Splits a list at the given element.
-- Element is in the second sublist after split.
splitAtEq :: Eq a => a -> [a] -> ([a],[a])
splitAtEq a [] = ([],[])
splitAtEq a ys@(x:xs)
    | a == x = ([],ys)
    | otherwise = let (as,bs) = splitAtEq a xs
                   in (x:as, bs)

-- Merge schedule 1 into schedule 2
-- sch1 values kept on collision as per Map.insert
mergeInto :: Schedule Label -> Schedule Label -> Schedule Label
mergeInto sch1 sch2 = recInsert (Map.toList sch1) sch2
    where
        recInsert [] sch = sch
        recInsert ((k,v):xs) sch =
            let sch' = Map.insert k v sch
             in recInsert xs sch'

-- |Creates a schedule where each 'Station' in
-- the given environment has an empty 'Stack'.
initSchedule :: Env -> Schedule Label
initSchedule env = Map.fromList
    [(st,[]) | st <- map stName (eStations env)]

-- Choose which leaf to schedule next from the list.
-- Uses shortest task followed by longest branch heuristics.
-- must take full tree
chooseLeaf :: [Label] -> Tree Label -> Map Label Recipe -> Label
chooseLeaf [] _ _ = error "No leaves"
chooseLeaf [l] _ _ = l
chooseLeaf ls fullTree rMap =
    let shortest = minimumBy (\l l' ->
            compare (duration l rMap) (duration l' rMap)) ls
        shorts = filter (\l -> duration shortest rMap == duration l rMap) ls
        bs = branches fullTree
        labelsWithBranch = map (\l -> (l, getBranch bs l)) shorts
     in if length shorts == 1
            then head shorts
            else fst $ maximumBy (\(_,b) (_,b') ->
                    compare (branchDuration b rMap) (branchDuration b' rMap)) labelsWithBranch

-- Given a list of branches, find the branch
-- containing the given label.
getBranch :: [[Label]] -> Label -> [Label]
getBranch [] _ = error "branch not found"
getBranch (b:bs) l
    | l `elem` b = b
    | otherwise = getBranch bs l

-- Sum the 'duration' of all labels in a branch.
branchDuration :: [Label] -> Map Label Recipe -> Time
branchDuration ls rMap = sum $
    map (\l -> duration l rMap) ls

-- Get the list of all the branches of a tree.
branches :: Tree Label -> [[Label]]
branches (Node a []) = [[a]]
branches (Node a ts) = [a : b | t <- ts
                              , let bs = branches t
                              , b <- bs]

-- Get a list of all labels at the leaves of the recipe
-- used to create the map.
leaves :: Tree Label -> Map Label Recipe -> [Label]
leaves (Node l []) _ = [l]
leaves (Node l ts) rMap = case Map.lookup l rMap of
    Just (Node (Transaction _) _) ->
        if concatMap subForest ts == []
            then [l]
            else let csOfDeps = concatMap subForest ts
                  in concatMap (\t -> leaves t rMap) csOfDeps
    _ -> concatMap (\t -> leaves t rMap) ts
