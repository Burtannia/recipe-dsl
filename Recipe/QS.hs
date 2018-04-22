{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Contains QuickCheck properties for testing
and code necessary to run QuickSpec on recipes.
-}

module Recipe.QS where

import           Control.Monad   (liftM, liftM2, liftM3)
import           Control.Monad.State
import           Data.List       (sort)
import           Data.List
import qualified Data.Map.Strict as Map (elems)
import           Data.Tree
import           QuickSpec
import           Recipe.Demo
import           Recipe.Kitchen
import           Recipe.Recipe
import           Recipe.Scheduler
import           Test.QuickCheck

-------------------------------------
-- Arbitrary Instances
-------------------------------------

instance {-# OVERLAPPING #-} Arbitrary Recipe where
    arbitrary = sized $ \n ->
        let un = resize (n-1) arbitrary
            bin = resize (n `div` 2) arbitrary
         in case n of
                0 -> genIng
                1 -> oneof
                    [genIng, genUnRec un]
                _ -> oneof
                    [genIng, genUnRec un, genBinRec bin bin]

-- |Generate an ingredient. Name is random between "1" and "100".
genIng :: Gen Recipe
genIng = liftM (ingredient . show) (choose (1, 100) :: Gen Int)

-- |Generate a unary recipe.
genUnRec :: Gen Recipe -> Gen Recipe
genUnRec un = oneof
    [ liftM heat un
    , liftM2 heatAt genTemp un
    , liftM2 heatFor arbitrary un
    , liftM wait un
    , liftM2 addCondition arbitrary (genUnRec un)
    , liftM transaction (genUnRec un)
    , liftM2 measure arbitrary un ]

-- |Generate a binary recipe.
genBinRec :: Gen Recipe -> Gen Recipe -> Gen Recipe
genBinRec r1 r2 = liftM3 combine genMethod r1 r2

-- |Generate a temperature between 100 and 240.
genTemp :: Gen Int
genTemp = choose (100, 240)

-- |Generate a combine method: "mix", "spread", "wrap".
genMethod :: Gen String
genMethod = elements
    [ "mix"
    , "spread"
    , "wrap" ]

instance Arbitrary Condition where
    arbitrary = sized $ \n ->
        let bin = resize (n `div` 2) arbitrary
         in if n < 2 then
                singleCond
            else
                oneof
                [ singleCond
                , liftM2 AND bin bin
                , liftM2 OR bin bin ]

-- |Generate a single condition (not AND or OR).
singleCond :: Gen Condition
singleCond = oneof
    [ liftM (CondOpt . show) (choose (1, 10) :: Gen Int)
    , liftM CondTime arbitrary
    , liftM CondTemp genTemp ]

instance Arbitrary Time where
    arbitrary = liftM Time (choose (1, 600))

instance Arbitrary Measurement where
    arbitrary = oneof
        [ liftM Count (elements [1..10])
        , liftM Grams (elements [10,20..1000])
        , liftM Milliletres (elements [10,20..1000]) ]

instance Arbitrary Obs where
    arbitrary = oneof
        [ liftM ObsTemp genTemp
        , liftM ObsTime arbitrary
        , liftM2 ObsOpt (liftM show (choose (1, 10) :: Gen Int)) arbitrary ]

instance Observe [Obs] Bool Condition where
    observe obs c = evalCond c obs

-- |Make an environment containing 3 useful stations
-- and 1 useless station.
mkUsefulEnv :: Env
mkUsefulEnv = evalState mkUsefulEnv' 0
    where
        mkUsefulEnv' :: State Int Env
        mkUsefulEnv' = do
            sts <- genSts 4
            let obs = [timeZero]
            return $ Env sts obs
        genSts :: Int -> State Int [Station]
        genSts 0 = return []
        genSts 2 = do
            i <- get
            let st = mkStationUseless (mkStName i)
            put (i + 1)
            sts <- genSts 1
            return (st : sts)
        genSts n = do
            i <- get
            let st = mkStationUseful (mkStName i)
            put (i + 1)
            sts <- genSts (n-1)
            return (st : sts)

-- |Make an environement containing 2 useless stations.
mkUselessEnv :: Env
mkUselessEnv =
    let sts = [ mkStationUseless (mkStName 0)
              , mkStationUseless (mkStName 1) ]
        obs = [timeZero]
     in Env sts obs

-- |ObsTime 0
timeZero :: IO Obs
timeZero = return $ ObsTime 0

-- |'mkStName' 1 = "Station_1"
mkStName :: Int -> StName
mkStName i = "Station_" ++ show i

-- |Make a station with the given name that can do everything.
mkStationUseful :: StName -> Station
mkStationUseful stNm =
    let constr r@(Node a ts) = case a of
            GetIngredient _ -> Just [Input, Output]
            Heat            -> Just [Input, Output]
            HeatAt t        -> Just [Input, Preheat t, Output]
            Combine s       -> Just [Input, PCombine s, Output]
            Wait            -> Just [Input, DoNothing, Output]
            Conditional _ c -> (constr $ popCond r)
                                >>= return . addEvalCond c
            Measure m       -> Just [Input, MeasureOut m, Output]
            Transaction a   -> constr $ popT r
     in Station stNm constr [return $ ObsTemp 20]

-- |Make a station with the given name that can't do anything.
mkStationUseless :: StName -> Station
mkStationUseless stNm = Station stNm (const Nothing) []

-------------------------------------
-- QuickSpec
-------------------------------------

qsRecipe = quickSpec
    [ con "ingredient" (ingredient :: String -> Recipe)
    , con "heatTo" (heatTo :: Int -> Recipe -> Recipe)
    , con "heatAt" (heatAt :: Int -> Recipe -> Recipe)
    , con "waitFor" (waitFor :: Time -> Recipe -> Recipe)
    , con "combine" (combine :: String -> Recipe -> Recipe -> Recipe)
    , con "addCondition" (addCondition :: Condition -> Recipe -> Recipe)
    , con ".&&" ((.&&) :: Condition -> Condition -> Condition)
    , con ".||" ((.||) :: Condition -> Condition -> Condition)
    , con "transaction" (transaction :: Recipe -> Recipe)
    , con "measure" (measure :: Measurement -> Recipe -> Recipe)

    , con "optional" (optional :: String -> Recipe -> Recipe)
    , con "toTemp" (toTemp :: Int -> Recipe -> Recipe)
    , con "forTime" (forTime :: Time -> Recipe -> Recipe)
    , con "hours" (hours :: Time -> Time)
    , con "minutes" (minutes :: Time -> Time)

    , monoType (Proxy :: Proxy Recipe)
    , monoType (Proxy :: Proxy Measurement)
    , monoTypeObserve (Proxy :: Proxy Condition)
    , monoType (Proxy :: Proxy Time) ]

-------------------------------------
-- QuickCheck Tests
-------------------------------------

-- |Takes two conditions and compares the result
-- of running 'evalCond' on each of them with
-- the given observables.
mkProp :: Condition -> Condition -> [Obs] -> Bool
mkProp lhs rhs obs =
    evalCond lhs obs == evalCond rhs obs

-- conditions

prop_and_comm x y obs =
    mkProp (x .&& y) (y .&& x) obs

prop_and_id x obs =
    mkProp (x .&& x) x obs

prop_or_comm x y obs =
    mkProp (x .|| y) (y .|| x) obs

prop_or_id x obs =
    mkProp (x .|| x) x obs

prop_and_assoc x y z obs =
    mkProp ((x .&& y) .&& z) (x .&& (y .&& z)) obs

prop_and_or_id x y obs =
    mkProp (x .&& (x .|| y)) x obs

prop_or_and_id x y obs =
    mkProp (x .|| (x .&& y)) x obs

prop_or_assoc x y z obs =
    mkProp ((x .|| y) .|| z) (x .|| (y .|| z)) obs

prop_quickspec_law_11 x y z obs =
    mkProp (x .&& (y .|| (x .&& z))) (x .&& (y .|| z)) obs

prop_and_distr_and x y z obs =
    mkProp ((x .&& y) .&& (x .&& z)) (x .&& (y .&& z)) obs

prop_and_distr_or x y z obs =
    mkProp ((x .&& y) .|| (x .&& z)) (x .&& (y .|| z)) obs

prop_or_distr_or x y z obs =
    mkProp ((x .|| y) .|| (x .|| z)) (x .|| (y .|| z)) obs

prop_or_distr_and x y z obs =
    mkProp ((x .|| y) .&& (x .|| z)) (x .|| (y .&& z)) obs

-- minutes and hours

prop_min_hours x =
    minutes (hours x) == hours (minutes x)

prop_min_min x =
    hours x == minutes (minutes x)

-- combinators

prop_combine_comm s r1 r2 =
    combine s r1 r2 == combine s r2 r1

-- labelling

prop_labelR_root r =
    let lTree = labelRecipeR r
        root = snd $ rootLabel lTree
     in root == r

prop_labelA_unlabel r =
    let lTree = labelRecipeA r
     in r == fmap snd lTree

-- fold

prop_fold_time r =
    let t = foldRecipe timeAction r
        tTree = fmap timeAction r
        t' = mconcat $ flatten tTree
     in t == t'

prop_fold_ing r =
    let is = sort $ ingredients r -- implemented with foldRecipe
        iTree = fmap (\a -> case a of
                                GetIngredient s -> [s]
                                _               -> []) r
        is' = (sort . mconcat . flatten) iTree
     in is == is'

-- evaluate conditions

prop_eval_cond_false_empty c =
    not $ evalCond c []

prop_eval_cond_false c obs =
    not $ evalCond c (failObs c obs)

-- |Generate observables for which a condition will fail.
failObs :: Condition -> [Obs] -> [Obs]
failObs c obs = filterTime t
    (filter (\o -> not $ o `elem` os) obs)
    where
        ts = getTimes c
        t = if ts == [] then
                0
            else
                minimum ts
        os = condToObs c
        filterTime t = filter (\o ->
            case o of
                ObsTime t' -> t' < t
                _          -> True)
        getTimes (CondTime t) = [t]
        getTimes (AND c1 c2)  = getTimes c1 ++ getTimes c2
        getTimes (OR c1 c2)   = getTimes c1 ++ getTimes c2
        getTimes _            = []

prop_eval_cond_true c obs =
    let os = condToObs c
     in evalCond c (obs ++ os)

-- |Generate observables for a condition to 'evalCond' to True.
condToObs :: Condition -> [Obs]
condToObs (CondTime t) = [ObsTime t]
condToObs (CondTemp t) = [ObsTemp t]
condToObs (CondOpt s)  = [ObsOpt s True]
condToObs (AND c1 c2)  = condToObs c1 ++ condToObs c2
condToObs (OR c1 c2)   = condToObs c1 ++ condToObs c2

-- Scheduling

prop_scheduling_succeed r =
    let lTree = labelRecipe r
        sch = scheduleRecipe r mkUsefulEnv
     in sort (getAllLabels sch) == sort (flatten lTree)

prop_scheduling_fail r =
    let lTree = labelRecipe r
        sch = scheduleRecipe r mkUselessEnv
     in expectFailure $
        sort (getAllLabels sch) == sort (flatten lTree)

getAllLabels :: Schedule Label -> [Label]
getAllLabels sch = concatMap getAllLabels' ts
    where
        ts = Map.elems sch
        getAllLabels' [] = []
        getAllLabels' (t:ts) =
            case t of
                Active l -> l : getAllLabels' ts
                _ -> getAllLabels' ts 

return []
runTests = $quickCheckAll
