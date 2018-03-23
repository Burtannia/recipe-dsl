module Recipe.Scheduler where

import           Control.Monad.LPMonad
import           Control.Monad.Trans.State
import           Data.LinearProgram
import           Data.LinearProgram.GLPK
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Recipe             hiding (Label)

-----------------------------
-- Label Recipe
-----------------------------

type Label = String

-- labels recipe tree with action1, action2...
mkLabelTree :: Recipe -> Tree (Label, Recipe)
mkLabelTree r = fmap (\(i,r) -> ("action" ++ show i, r)) lr
    where lr = labelRecipeR r

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
-- LF Helper Functions
-----------------------------

mkLF :: String -> LinFunc Label Int
mkLF s = linCombination [(1,s)]

-- s1 + s2
mkLF2 :: String -> String -> LinFunc String Int
mkLF2 s1 s2 = linCombination [(1,s1), (1,s2)]

-- x + x1 + x2 ...
mkLFN :: String -> [String] -> LinFunc String Int
mkLFN x xs = let ones = replicate (length xs) 1
                 ys   = (1,x) : zip ones xs
              in linCombination ys

type Prefix = String

mkVar :: Prefix -> Label -> String
mkVar p l = p ++ "_" ++ l

mkVars :: Prefix -> [Label] -> [String]
mkVars p = map (mkVar p)

mkVar2 :: Prefix -> (Label, [StName]) -> [String]
mkVar2 p (l, sts) = [p ++ "_" ++ l ++ "_" ++ st | st <- sts]

mkVars2 :: Prefix -> [(Label, [StName])] -> [String]
mkVars2 p = concatMap (mkVar2 p)

-----------------------------
-- Constraints
-----------------------------

maxInt = maxBound :: Int

-- objective function = total time
totalTime :: Env -> Map Label Recipe -> LinFunc String Int
totalTime env rMap = let l = maximum $ Map.keys rMap
                         r = lookupR l rMap
                         sts = validStations env r
                         (v:vs) = mkVar2 "end" (l,sts)
                      in mkLFN v vs

mkRecSts :: Prefix -> Env -> (Label, Recipe) -> [LinFunc String Int]
mkRecSts p env (l,r) = let sts  = validStations env r
                           vars = mkVar2 p (l,sts)
                        in map mkLF vars

startRecSts :: Env -> Map Label Recipe -> [LinFunc String Int]
startRecSts env rMap = concatMap (startRecSts' env) (Map.toList rMap)

startRecSts' :: Env -> (Label, Recipe) -> [LinFunc String Int]
startRecSts' = mkRecSts "start"

endRecSts' :: Env -> (Label, Recipe) -> [LinFunc String Int]
endRecSts' = mkRecSts "end"

dependencies :: Env -> Tree Label -> Map Label Recipe -> [(LinFunc String Int, LinFunc String Int)]
dependencies env lTree rMap = let f = \(l,r) -> (startRecSts' env (l,r), childLabels l lTree)
                                  xs = map f (Map.toList rMap) -- ([LF], [Label])
                                  g = \(starts,cs) -> [(start, e) | start <- starts
                                                                  , c <- cs
                                                                  , let es = endRecSts' env (c, lookupR c rMap)
                                                                  , e <- es] -- [(LF,LF)]
                               in concatMap g xs

-----------------------------
-- Running GLPK
-----------------------------

lp :: Recipe -> Env -> LP String Int
lp r env = execLPM $ do
    let lrTree = mkLabelTree r
    let lTree = fmap fst lrTree
    let rMap = mkLabelMap lrTree

    setDirection Min
    setObjective (totalTime env rMap)

    -- forall valid stations and recipes start_recipe_station >= 0
    mapM (\f -> f `geqTo` 0) (startRecSts env rMap)

    -- recipe cannot start before children finish
    -- forall r, forall st, start >= forall cs, forall st, end
    mapM (\(x,y) -> x `geq` y) (dependencies env lTree rMap)


    -- end = start + duration
    -- start = bin_rec_st * start_rec_st
    -- if recipe is transaction, start_rec_st = end of children


    -- end_rec_st = bin_rec_st * (start_rec_st + dur_rec)

    -- bin_rec_st1 + bin_rec_st2 + ... = 1


-- lp :: LP String Int
-- lp = execLPM $ do
--     setDirection Min
--     setObjective (linCombination [(10,"x1")])--(totalTime labelSet)
--     -- constraints etc.
--     leqTo (linCombination [(10,"x1")]) 300
--     varGeq "x1" 10
--     setVarKind "b1" BinVar

-- test = print =<< glpSolveVars mipDefaults lp
