{-# LANGUAGE RecordWildCards #-}

{-|
Allows the simulation of a recipe in the given environment,
printing progress to sdout.
-}

module Recipe.Simulator (simulate) where

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
import System.Random

data Simulator = Simulator
                { sCompletes :: [Label]
                , sLRecipe :: Tree (Label, Recipe)
                , sEnv :: Env
                , sSpeed :: Float
                , sClr :: Bool -- should clear stdout after each round of steps?
                , sSchedule :: SchList }

data ProcessStatus = PCompleted | PIncomplete
    deriving (Show, Eq)

data TaskStatus = TCompleted
                | TNotStarted
                | TInProgress LProcess
                deriving (Show, Eq)

-- Label of action, Input labels, processes to perform.
type LProcess = (Label, [Label], [(ProcessStatus, Process)])

type SchList = [(StName, [(TaskStatus, Task Label)])]

-- |Schedule the given recipe in the given environment
-- then simulate it with the given speed multiplier.
-- Also takes a Bool to determine whether to clear
-- stdout after each round of tasks (True means clear).
simulate :: Recipe -> Env -> Float -> Bool -> IO ()
simulate r env spd clr =
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
                        , sClr = clr
                        , sSchedule = schWithStatus }
     in runSimulator sim

-- Remove completed tasks from the stations in the simulator.
pruneCompleteT :: Simulator -> Simulator
pruneCompleteT Simulator{..} =
    let sch = map (\(st, ts) -> (st, remComp ts)) sSchedule
        remComp ts = [(state, t) | (state, t) <- ts
                                 , not $ state == TCompleted]
     in Simulator sCompletes sLRecipe sEnv sSpeed sClr sch

-- Remove stations with no tasks left from the simulator.
pruneCompleteSt :: Simulator -> Simulator
pruneCompleteSt Simulator{..} =
    let sch = filter (\(st,ts) -> length ts > 0) sSchedule
     in Simulator sCompletes sLRecipe sEnv sSpeed sClr sch

pruneSch :: Simulator -> Simulator
pruneSch = pruneCompleteSt . pruneCompleteT

-- Check if all stations have run out of tasks
-- in the simulator.
tasksEmpty :: Simulator -> Bool    
tasksEmpty Simulator{..} =
    sSchedule == []

-- Clears stdout
clearStdout :: IO ()
clearStdout = putStr "\ESC[2J"

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
            if sClr then
                clearStdout
            else
                putStrLn ""
            (env, compls, sch) <- runSchedule sEnv sLRecipe sCompletes sSchedule
            let obs = incTime (eObs env)
            let env' = Env (eStations env) obs
            let uSeconds = (1 / sSpeed) * 1000000
            delay (floor uSeconds)
            return $ Simulator compls sLRecipe env' sSpeed sClr sch

-- Print a list of IO observables
-- used for debugging
printObs :: [IO Obs] -> IO ()
printObs [] = return ()
printObs (x:xs) = do
    y <- x
    print y
    printObs xs

-- Applies 'incTime'' to all observables
-- in a list.
incTime :: [IO Obs] -> [IO Obs]
incTime = map incTime'

-- Increment an 'ObsTime' by 1,
-- does nothing to other 'Obs'.
incTime' :: IO Obs -> IO Obs
incTime' o = do
    obs <- o
    case obs of
        ObsTime t -> return $ ObsTime (t+1)
        _ -> return obs

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

-- Given a list of the global observables,
-- return the observable over time ('ObsTime').
-- Throws an error if it doesn't exist.
getGlobalTime :: [IO Obs] -> IO Obs
getGlobalTime [] = error "No time observable"
getGlobalTime (o:os) = do
    obs <- o
    case obs of
        ObsTime t -> return obs
        _ -> getGlobalTime os

-- Runs the given task returning an updated environment, list of completed tasks and updated task.
runTask :: Env -> StName -> Tree (Label, Recipe) -> [Label] -> (TaskStatus, Task Label) -> IO (Env, [Label], (TaskStatus, Task Label))
runTask env stNm lTree compls (status, t) = do
    tObs@(ObsTime gTime) <- getGlobalTime (eObs env)
    let Station{..} = findStation stNm (eStations env)
    case status of
        TCompleted -> return (env, compls, (status, t))
        TNotStarted -> case t of
            Active l -> do
                let et = expandActive (fromJust . stConstrF) t lTree tObs -- :: LProcess
                runActive et gTime
            Idle time -> do
                printStName gTime
                let t' = Idle (time - 1)
                putStrLn $ "Idle - Time remaining: " ++ show time
                if (time <= 1) then
                    return (env, compls, (TCompleted, t'))
                else
                    return (env, compls, (TNotStarted, t')) -- idle doesn't have processes so no need to mark as InProgress
        TInProgress et -> runActive et gTime
    where
        runActive et gTime = do 
            printStName gTime
            (env', compls', et') <- runProcesses stNm env compls et
            let status' = mkTaskStatus et'
            return (env', compls', (status', t))
        printStName gTime = putStrLn $ (show gTime) ++ " - " ++ stNm ++ ":"

-- End: Output reached, check for conditions to
-- either loop or terminate.
-- Terminate: End task and mark all processes completed.
-- Continue: Run as usual on next cycle.
-- Next: Run the next process immediately.
data Result = Continue | End | Terminate | Next

-- Runs an 'LProcess' for the given 'Station'. Takes a list
-- of completed labels and returns an updated environment,
-- updated list of completed labels and an updated 'LProcess'.
-- If dependencies are not avialable, will print out a message
-- and return.
runProcesses :: StName -> Env -> [Label] -> LProcess -> IO (Env, [Label], LProcess)
runProcesses stNm env compls lp@(l, deps, []) = return (env, compls, lp)
runProcesses stNm env compls lp@(l, deps, ps) = do
    let st@Station{..} = findStation stNm (eStations env)
    let incompDeps = depsIncomplete deps compls
    if length incompDeps > 0 then do
        putStrLn $ "Waiting for dependencies: " ++ show incompDeps
        let stats = map fst ps
        let procs = map snd ps
        let procs' = incTimeConds procs -- delay conditions for a second
        let ps' = zip stats procs'
        return (env, compls, (l, deps, ps'))
    else do
        globals <- sequence $ eObs env
        locals <- sequence stObs
        (locals', ps', result) <- runProcesses' globals locals ps
        let newSt = Station stName stConstrF (map return locals')
        let env' = updateStation newSt env
        let lp' = (l, deps, ps')
        case result of
            Next -> runProcesses stNm env' compls lp'
            Continue -> return (env', compls, lp')
            End -> case getCond ps of
                     Nothing -> output env' lp'
                     Just c -> if isOpt c then
                                   output env' lp'
                               else
                                   runProcesses stNm env' compls (l, deps, setupRerun ps')
            Terminate -> do
                let lp'' = markAllComplete lp'
                output env' lp''
    where
        output env' lp' = do
            putStrLn $ "Output: " ++ show l
            return (env', l : compls, lp')

        runProcesses' :: [Obs] -> [Obs] -> [(ProcessStatus, Process)] -> IO ([Obs], [(ProcessStatus, Process)], Result)
        runProcesses' _ locals [] = return (locals, [], Terminate)
        runProcesses' globals locals ((status, p) : ps) = do
            case status of

                PCompleted -> do
                    (locals', ps', r) <- runProcesses' globals locals ps
                    return (locals', (status, p) : ps', r)

                PIncomplete -> case p of
                    Input ->
                        if length deps > 0 then do
                            putStrLn $ "Receiving Inputs: " ++ show deps
                            return (locals, (PCompleted, p) : ps, Continue)
                        else
                            return (locals, (PCompleted, p) : ps, Next)

                    Output ->
                        return (locals, (PCompleted, p) : ps, End)
                        -- output printed after conditions evaluated
                        -- once this function returns as the processes
                        -- may need to be run again before output

                    Fetch s -> do
                        putStrLn $ "Fetching " ++ s
                        i <- randomRIO (1,7) :: IO Int
                        if i == 7 then -- fetching an ingredient takes, on average, 7 cycles (seconds)
                            return (locals, (PCompleted, p) : ps, Continue)
                        else
                            return (locals, (status, p) : ps, Continue)

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
                        let obs = globals ++ locals
                        result <- evalCondPrint c obs

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
                                    else case listToMaybe temps of
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
                terminate = return (locals, (PCompleted, p) : ps, Terminate)
                continuePs = return (locals, (PCompleted, p) : ps, Continue)

-- Marks all 'Process'es within an 'LProcess'
-- as 'PCompleted'.
markAllComplete :: LProcess -> LProcess
markAllComplete (l, deps, ps) =
    let ps' = map (\(_,p) -> (PCompleted, p)) ps
     in (l, deps, ps')

-- Evaluate a condition given a list of observables
-- returns IO result after printing result.
evalCondPrint :: Condition -> [Obs] -> IO Bool
evalCondPrint c obs = do
    putStrLn $ "Evaluating condition: " ++ show c
    let result = evalCond c obs
    putStrLn $ "Condition evaluated " ++ show result
    return result

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

-- Replaces the corresponding station in the environment
-- with the given station.
updateStation :: Station -> Env -> Env
updateStation st Env{..} =
    let eSts = filter (\st' ->
            not $ stName st == stName st') eStations
     in Env (st : eSts) eObs

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
-- Also changes an CondTime conditions into relative time therefore
-- 'expandActive' must be passed the global time observable.
expandActive :: (Recipe -> [Process]) -> Task Label -> Tree (Label, Recipe) -> Obs -> LProcess
expandActive f (Active l) lTree o =
    let r = fromJust $ lookup l (flatten lTree)
        is = childLabels l (fmap fst lTree)
        ps = condsToAbsolute (f r) o
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
flipStacks :: Schedule a -> Schedule a
flipStacks = Map.map reverse