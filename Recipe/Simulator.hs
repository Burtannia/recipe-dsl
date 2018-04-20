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

pruneCompleteT :: Simulator -> Simulator
pruneCompleteT Simulator{..} =
    let sch = map (\(st, ts) -> (st, remComp ts)) sSchedule
        remComp ts = [(state, t) | (state, t) <- ts
                                 , not $ state == TCompleted]
     in Simulator sCompletes sLRecipe sEnv sSpeed sch

pruneCompleteSt :: Simulator -> Simulator
pruneCompleteSt Simulator{..} =
    let sch = filter (\(st,ts) -> length ts > 0) sSchedule
     in Simulator sCompletes sLRecipe sEnv sSpeed sch

pruneSch :: Simulator -> Simulator
pruneSch = pruneCompleteSt . pruneCompleteT

tasksEmpty :: Simulator -> Bool    
tasksEmpty Simulator{..} =
    sSchedule == []

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

incTime' :: IO Obs -> IO Obs
incTime' o = do
    obs <- o
    case obs of
        ObsTime t -> return $ ObsTime (t+1)
        _ -> o

runSchedule :: Env -> Tree (Label, Recipe) -> [Label] -> SchList -> IO (Env, [Label], SchList)
runSchedule env _ compls [] = return (env, compls, [])
runSchedule env lTree compls (s@(st,ts):ss) =
    case ts of
        [] -> do 
            (env', compls', ss') <- runSchedule env lTree compls ss
            return (env', compls', s : ss')
        (t:ts) -> do
            (env', compls', t') <- runTask env st lTree compls t
            let s' = (st, t' : ts)
            (env'', compls'', ss') <- runSchedule env' lTree compls' ss
            return (env'', compls'', s' : ss')

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
                let t' = Idle (time - 1)
                return (env, compls, (TNotStarted, t')) -- idle doesn't have processes so no need to mark as InProgress
        TInProgress et -> runActive et
    where
        runActive et = do 
            (env', compls', et') <- runProcesses stNm env compls et
            let status' = mkTaskStatus et'
            return (env', compls', (TInProgress et', t))

data Result = Continue | End | Terminate

runProcesses :: StName -> Env -> [Label] -> LProcess -> IO (Env, [Label], LProcess)
runProcesses stNm env compls lp@(l, deps, []) = return (env, compls, lp)
runProcesses stNm env compls lp@(l, deps, ps) = do
    let incompDeps = depsIncomplete deps compls
    if length incompDeps > 0 then do
        putStrLn $ "Waiting for dependencies: " ++ show incompDeps
        return (env, compls, lp)
    else do
        let st@Station{..} = findStation stNm (eStations env)
        putStrLn $ stNm ++ ":"
        globals <- sequence $ eObs env
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
                                    else do
                                        let locals' = if currTemp < (extractTemp temps) then
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

getCond :: [(ProcessStatus, Process)] -> Maybe Condition
getCond = getCond' . map snd
    where
        getCond' ps = listToMaybe [c | (EvalCond c) <- ps]

setupRerun :: [(ProcessStatus, Process)] -> [(ProcessStatus, Process)]
setupRerun [] = []
setupRerun ((stat, p) : ps) =
    case p of
        Input -> (stat, p) : setupRerun ps
        Preheat t -> (stat, p) : setupRerun ps
        _ -> (PIncomplete, p) : setupRerun ps

isOpt :: Condition -> Bool
isOpt (CondOpt _) = True
isOpt (AND c1 c2) = isOpt c1 || isOpt c2
isOpt (OR c1 c2) = isOpt c1 || isOpt c2
isOpt _ = False

updateStation :: Station -> Env -> Env
updateStation st Env{..} =
    let eSts = filter (\st' -> not $ stName st == stName st') eStations
     in Env (st : eSts) eObs

extractTemp :: [Condition] -> Int
extractTemp [] = 0
extractTemp (c:cs) =
    let extractTemp' (CondTemp t) = Sum t
        extractTemp' _ = Sum 0
        t = getSum $ foldCond extractTemp' c
     in if t == 0 then
            extractTemp cs
        else
            t

extractTemps :: Condition -> [Condition]
extractTemps (CondTemp t) = [CondTemp t]
extractTemps (AND c1 c2) = extractTemps c1 ++ extractTemps c2
extractTemps (OR c1 c2) = extractTemps c1 ++ extractTemps c2
extractTemps _ = []

extractOpts :: Condition -> [Condition]
extractOpts (CondOpt s) = [CondOpt s]
extractOpts (AND c1 c2) = extractOpts c1 ++ extractOpts c2
extractOpts (OR c1 c2) = extractOpts c1 ++ extractOpts c2
extractOpts _ = []

getTemp :: [Obs] -> Int
getTemp os = let ts = [t | ObsTemp t <- os]
              in if ts == [] then
                    error "No Observable Temperature"
                 else
                    head ts

incTemp :: [Obs] -> [Obs]
incTemp [] = []
incTemp ((ObsTemp t) : os) = ObsTemp (t+1) : incTemp os
incTemp (o:os) = o : incTemp os

decTemp :: [Obs] -> [Obs]
decTemp [] = []
decTemp ((ObsTemp t) : os) = ObsTemp (t-1) : decTemp os
decTemp (o:os) = o : decTemp os

getConds :: [Process] -> [Process]
getConds ps = [x | x@(EvalCond _) <- ps]

mkTaskStatus :: LProcess -> TaskStatus
mkTaskStatus et@(_, _, ps) =
    let cs = filter (\(st,_) -> not $ st == PCompleted) ps
        len = length cs
     in if len == 0 then
            TCompleted
        else
            TInProgress et

toLProcess :: (Recipe -> [Process]) -> Label -> Tree (Label, Recipe) -> LProcess
toLProcess f l lTree =
    let r = fromJust $ lookup l (flatten lTree)
        is = childLabels l (fmap fst lTree)
        ps = f r
        ps' = map (\p -> (PIncomplete, p)) ps
     in (l, is, ps')

expandActive :: (Recipe -> [Process]) -> Task Label -> Tree (Label, Recipe) -> LProcess
expandActive f (Active l) lTree =
    let r = fromJust $ lookup l (flatten lTree)
        is = childLabels l (fmap fst lTree)
        ps = f r
        ps' = map (\p -> (PIncomplete, p)) ps
     in (l, is, ps')

depsIncomplete :: [Label] -> [Label] -> [Label]
depsIncomplete deps compls =
    let deps' = map (\l -> (l, l `elem` compls)) deps
     in [l | (l, False) <- deps']

-- add multiplier

popStack :: Stack a -> Stack a
popStack [] = []
popStack (x:xs) = xs

findStation :: StName -> [Station] -> Station
findStation stNm = head . filter (\Station{..} -> stName == stNm)

flipStacks :: Schedule Label -> Schedule Label
flipStacks = Map.map reverse