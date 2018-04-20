{-# LANGUAGE RecordWildCards #-}

module Recipe.Simulator where

import Recipe.Recipe
import Recipe.Kitchen
import Recipe.Scheduler
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Tree
import Control.Monad (liftM)
import Data.Monoid
import Control.Concurrent.Thread.Delay (delay)

data Simulator = Simulator
                { sCompletes :: [Label]
                , sLRecipe :: Tree (Label, Recipe)
                , sEnv :: Env
                , sSpeed :: Float
                , sSchedule :: SchList }

data ProcessStatus = PCompleted | PIncomplete
    deriving (Show, Eq)

data TaskStatus = TCompleted
                | TNotStarted
                | TInProgress LProcess
                deriving (Show, Eq)

-- |Label of action, Input labels, processes to perform.
type LProcess = (Label, [Label], [(ProcessStatus, Process)])

type SchList = [(StName, [(TaskStatus, Task Label)])]

-- |Schedule the given recipe in the given environment
-- then simulate it with the given speed multiplier.
simulate :: Recipe -> Env -> Float -> IO ()
simulate r env spd =
    let lTree = labelRecipeR r
        sch = flipStacks $ scheduleRecipe r env
        schList = Map.toList sch
        schWithStatus = map (\(st, ts) ->
            (st, map (\t ->
                (TNotStarted, t)) ts)) schList
        sim = Simulator { sCompletes = []
                        , sLRecipe = lTree 
                        , sEnv = env
                        , sSpeed = spd
                        , sSchedule = schWithStatus }
     in runSimulator sim

-- Remove completed tasks from the stations in the simulator.
pruneCompleteT :: Simulator -> Simulator
pruneCompleteT Simulator{..} =
    let sch = map (\(st, ts) -> (st, remComp ts)) sSchedule
        remComp ts = [(state, t) | (state, t) <- ts
                                 , not $ state == TCompleted]
     in Simulator sCompletes sLRecipe sEnv sSpeed sch

-- Remove stations with no tasks left from the simulator.
pruneCompleteSt :: Simulator -> Simulator
pruneCompleteSt Simulator{..} =
    let sch = filter (\(st,ts) -> length ts > 0) sSchedule
     in Simulator sCompletes sLRecipe sEnv sSpeed sch

pruneSch :: Simulator -> Simulator
pruneSch = pruneCompleteSt . pruneCompleteT

-- Check if all stations have run out of tasks
-- in the simulator.
tasksEmpty :: Simulator -> Bool    
tasksEmpty Simulator{..} =
    sSchedule == []

-- Runs one pass of the simulator (1 second adjusted depending
-- on the speed passed to 'simulate'). Prunes completed
-- tasks and stations and repeats until all are completed.
runSimulator :: Simulator -> IO ()
runSimulator s = do
    s' <- liftM pruneSch $ runSimulator' s
    if tasksEmpty s then
        putStrLn "Simulation Finished"
    else
        runSimulator s'
    where
        runSimulator' Simulator{..} = do
            (env, compls, sch) <- runSchedule sEnv sLRecipe sCompletes sSchedule
            let obs = incTime (eObs env)
            let env' = Env (eStations env) obs
            let uSeconds = (1 / sSpeed) * 1000000
            delay (floor uSeconds)
            return $ Simulator compls sLRecipe env' sSpeed sch

incTime :: [IO Obs] -> [IO Obs]
incTime = map incTime'

-- Increment an 'ObsTime' by 1,
-- does nothing to other 'Obs'.
incTime' :: IO Obs -> IO Obs
incTime' o = do
    obs <- o
    case obs of
        ObsTime t -> return $ ObsTime (t+1)
        _ -> o

-- Runs a single pass over a schedule, runs the first incomplete task of every station.
runSchedule :: Env -> Tree (Label, Recipe) -> [Label] -> SchList -> IO (Env, [Label], SchList)
runSchedule env _ compls [] = return (env, compls, [])
runSchedule env lTree compls (s@(st,ts):ss) = runSchedule' ts
    where
        runSchedule' [] = do
            (env', compls', ss') <- runSchedule env lTree compls ss
            return (env', compls', s : ss')
        runSchedule' (t:ts) =
            case fst t of
                TCompleted -> runSchedule' ts
                _ -> do
                    (env', compls', t') <- runTask env st lTree compls t
                    let s' = (st, t' : ts)
                    (env'', compls'', ss') <- runSchedule env' lTree compls' ss
                    return (env'', compls'', s' : ss')

-- Runs the given task returning an updated environment, list of completed tasks and updated task.
runTask :: Env -> StName -> Tree (Label, Recipe) -> [Label] -> (TaskStatus, Task Label) -> IO (Env, [Label], (TaskStatus, Task Label))
runTask env stNm lTree compls (status, t) = do
    let Station{..} = findStation stNm (eStations env)
    case status of
        TCompleted -> return (env, compls, (status, t))
        TNotStarted -> case t of
            Active l -> do
                let et = expandActive (fromJust . stConstrF) t lTree -- :: LProcess
                runActive et
            Idle time -> do
                putStrLn $ stNm ++ ":"
                let t' = Idle (time - 1)
                putStrLn $ "Idle - Time remaining: " ++ show time
                if (time <= 1) then
                    return (env, compls, (TCompleted, t'))
                else
                    return (env, compls, (TNotStarted, t')) -- idle doesn't have processes so no need to mark as InProgress
        TInProgress et -> runActive et
    where
        runActive et = do 
            putStrLn $ stNm ++ ":"
            (env', compls', et') <- runProcesses stNm env compls et
            let status' = mkTaskStatus et'
            return (env', compls', (status', t))

data Result = Continue | End | Terminate

runProcesses :: StName -> Env -> [Label] -> LProcess -> IO (Env, [Label], LProcess)
runProcesses stNm env compls lp@(l, deps, []) = return (env, compls, lp)
runProcesses stNm env compls lp@(l, deps, ps) = do
    let st@Station{..} = findStation stNm (eStations env)
    let incompDeps = depsIncomplete deps compls
    if length incompDeps > 0 then do
        putStrLn $ "Waiting for dependencies: " ++ show incompDeps
        return (env, compls, lp)
    else do
        putStrLn "eval globals"
        globals <- sequence $ eObs env
        putStrLn "eval locals"
        locals <- sequence stObs
        (locals', ps', result) <- runProcesses' globals locals ps
        let newSt = Station stName stConstrF (map return locals')
        let env' = updateStation newSt env
        let lp' = (l, deps, ps')
        case result of
            Continue -> return (env', compls, lp')
            End -> case getCond ps of
                     Nothing -> return (env', l : compls, lp')
                     Just c ->
                        if evalCond c (globals ++ locals) then
                            return (env', l : compls, lp')
                        else
                            if isOpt c then
                                return (env', l : compls, lp')
                            else
                                return (env', compls, (l, deps, setupRerun ps'))
            Terminate -> return (env', l : compls, lp')
    where
        runProcesses' :: [Obs] -> [Obs] -> [(ProcessStatus, Process)] -> IO ([Obs], [(ProcessStatus, Process)], Result)
        runProcesses' _ locals [] = return (locals, [], Terminate)
        runProcesses' globals locals ((status, p) : ps) =
            case status of

                PCompleted -> do
                    (locals', ps', r) <- runProcesses' globals locals ps
                    return (locals', (status, p) : ps', r)

                PIncomplete -> case p of
                    Input -> do
                        putStrLn $ "Receiving Inputs: " ++ show deps
                        return (locals, (PCompleted, p) : ps, Continue)

                    Output -> do
                        putStrLn $ "Output: " ++ show l
                        return (locals, (PCompleted, p) : ps, End)

                    Preheat t -> do
                        let currTemp = getTemp locals
                        putStrLn ("Preheating to " ++ show t
                                    ++ ", current temperature is "
                                    ++ show (getTemp locals))
                        let locals' = if currTemp < t then
                                          incTemp locals
                                      else
                                          decTemp locals
                        if currTemp == t then
                            return (locals', (PCompleted, p) : ps, Continue)
                        else
                            return (locals', (PIncomplete, p) : ps, Continue)

                    DoNothing -> do
                        putStrLn $ "Holding Inputs"
                        return (locals, (PCompleted, p) : ps, Continue)

                    PCombine s -> do
                        putStrLn $ "Combining inputs (" ++ s ++ ")"
                        return (locals, (PCompleted, p) : ps, Continue)

                    EvalCond c -> do
                        putStrLn $ "Evaluating condition: " ++ show c
                        let obs = globals ++ locals
                        let result = evalCond c obs
                        putStrLn $ "Condition evaluated " ++ show result

                        let opts = extractOpts c
                        let temps = extractTemps c
                        let currTemp = getTemp locals
                        if length opts > 0 then
                            if True `elem` map (\c -> evalCond c obs) opts then
                                continuePs
                            else
                                terminate
                        else
                            if result then
                                terminate
                            else
                                if length temps > 0 then
                                    if True `elem` map (\c -> evalCond c obs) opts then
                                        continuePs
                                    else case extractTemp temps of
                                        Nothing -> error "No Temperatures"
                                        Just x -> do
                                            let locals' = if currTemp < x then
                                                            incTemp locals
                                                        else
                                                            decTemp locals
                                            return (locals', (PCompleted, p) : ps, Continue)
                                else
                                    continuePs

                    MeasureOut m -> do
                        putStrLn $ "Measuring " ++ show m ++ "of inputs"
                        return (locals, (PCompleted, p) : ps, Continue)
            where
                terminate = do
                    putStrLn $ "Output: " ++ show l
                    let ps' = map (\(_,p) -> (PCompleted,p)) ps
                    return (locals, (PCompleted, p) : ps', Terminate)
                continuePs = return (locals, (PCompleted, p) : ps, Continue)

printObs :: [IO Obs] -> IO ()
printObs [] = return ()
printObs (x:xs) = do
    y <- x
    print y
    printObs xs

-- Gets the first process matching 'EvalCond', returns
-- Nothing if there are none.
getCond :: [(ProcessStatus, Process)] -> Maybe Condition
getCond = getCond' . map snd
    where
        getCond' ps = listToMaybe [c | (EvalCond c) <- ps]

-- Setup a set of tasks to be rerurn, maps over them setting their
-- statuses to 'PIncomplete'. Ignores 'Input' and 'Preheat' as
-- presumably we only want to do this the first time.
-- Intended to be used for looping over 'Process'es after 'EvalCond'.
setupRerun :: [(ProcessStatus, Process)] -> [(ProcessStatus, Process)]
setupRerun [] = []
setupRerun ((stat, p) : ps) =
    case p of
        Input -> (stat, p) : setupRerun ps
        Preheat t -> (stat, p) : setupRerun ps
        _ -> (PIncomplete, p) : setupRerun ps

-- Returns True if a condition contains a 'CondOpt'.
isOpt :: Condition -> Bool
isOpt (CondOpt _) = True
isOpt (AND c1 c2) = isOpt c1 || isOpt c2
isOpt (OR c1 c2) = isOpt c1 || isOpt c2
isOpt _ = False

-- Replaces the corresponding station in the environment
-- with the given station.
updateStation :: Station -> Env -> Env
updateStation st Env{..} =
    let eSts = filter (\st' -> not $ stName st == stName st') eStations
     in Env (st : eSts) eObs

-- Extracts the temperature from a list of conditions.
-- Returns the first temperature it finds else Nothing.
extractTemp :: [Condition] -> Maybe Int
extractTemp [] = Nothing
extractTemp (c:cs) = case extractTemp' c of
    Nothing -> extractTemp cs
    x -> x
    where
        extractTemp' (CondTemp t) = Just t
        extractTemp' (AND c1 c2) = extractAndOr c1 c2
        extractTemp' (OR c1 c2) = extractAndOr c1 c2
        extractTemp' _ = Nothing
        extractAndOr c1 c2 = case extractTemp' c1 of
            Nothing -> extractTemp' c2
            x -> x

-- Extracts all 'CondTemp's from a given condition.
extractTemps :: Condition -> [Condition]
extractTemps (CondTemp t) = [CondTemp t]
extractTemps (AND c1 c2) = extractTemps c1 ++ extractTemps c2
extractTemps (OR c1 c2) = extractTemps c1 ++ extractTemps c2
extractTemps _ = []

-- Extracts all 'CondOpt's from a given condition.
extractOpts :: Condition -> [Condition]
extractOpts (CondOpt s) = [CondOpt s]
extractOpts (AND c1 c2) = extractOpts c1 ++ extractOpts c2
extractOpts (OR c1 c2) = extractOpts c1 ++ extractOpts c2
extractOpts _ = []

-- Gets the temperature from a list of observables.
-- Throws error if there are no 'ObsTemp's.
getTemp :: [Obs] -> Int
getTemp os = let ts = [t | ObsTemp t <- os]
              in if ts == [] then
                    error "No Observable Temperature"
                 else
                    head ts

-- Increments all 'ObsTemp's in a list of observables.
-- Does nothing to other observables.
incTemp :: [Obs] -> [Obs]
incTemp [] = []
incTemp ((ObsTemp t) : os) = ObsTemp (t+1) : incTemp os
incTemp (o:os) = o : incTemp os

-- Decrements all 'ObsTemp's in a list of observables.
-- Does nothing to other observables.
decTemp :: [Obs] -> [Obs]
decTemp [] = []
decTemp ((ObsTemp t) : os) = ObsTemp (t-1) : decTemp os
decTemp (o:os) = o : decTemp os

-- Returns a list of all 'EvalCond' processes
-- in a list of processes.
getConds :: [Process] -> [Process]
getConds ps = [x | x@(EvalCond _) <- ps]

-- Determines the status of a task given its list
-- of processes.
mkTaskStatus :: LProcess -> TaskStatus
mkTaskStatus et@(_, _, ps) =
    let cs = filter (\(stat,_) -> not $ stat == PCompleted) ps
        len = length cs
     in if len == 0 then
            TCompleted
        else
            TInProgress et

-- Expands an active task into an 'LProcess'.
expandActive :: (Recipe -> [Process]) -> Task Label -> Tree (Label, Recipe) -> LProcess
expandActive f (Active l) lTree =
    let r = fromJust $ lookup l (flatten lTree)
        is = childLabels l (fmap fst lTree)
        ps = f r
        ps' = map (\p -> (PIncomplete, p)) ps
     in (l, is, ps')

-- Given a list of dependencies and a list of completed
-- task labels, return a list of all dependencies
-- not completed.
depsIncomplete :: [Label] -> [Label] -> [Label]
depsIncomplete deps compls =
    let deps' = map (\l -> (l, l `elem` compls)) deps
     in [l | (l, False) <- deps']

-- Find the station with the given name in the given list.
findStation :: StName -> [Station] -> Station
findStation stNm = head . filter (\Station{..} -> stName == stNm)

-- Reverse the order of the stacks in the schedule.
flipStacks :: Schedule Label -> Schedule Label
flipStacks = Map.map reverse