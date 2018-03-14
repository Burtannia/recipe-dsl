module Recipe.Scheduler where

import Recipe.Recipe
import Recipe.Kitchen
import Data.LinearProgram
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tree
import Control.Monad.Trans.State
import Data.Maybe (isJust)

-------------------------
-- Gen Labels
-------------------------

-- action1, action2...
mkLabelTree :: Recipe -> Tree (String, Action)
mkLabelTree r = fmap (\(i,a) -> ("action" ++ show i, a)) lr
    where lr = labelRecipe r

-- variation of mkLabelTree which keeps the entire recipe
-- alongside the labelled action
mkLabelTreeR :: Recipe -> Tree (String, Recipe)
mkLabelTreeR r = fmap (\(i,r) -> ("action" ++ show i, r)) lr
    where lr = labelRecipeR r

mkLabelSet :: Recipe -> Map String Action
mkLabelSet r = let lr = mkLabelTree r
                in Map.fromList (flatten lr)

childLabels :: String -> Tree String -> [String]
childLabels s (Node a ts)
    | a == s    = map rootLabel ts
    | otherwise = concatMap (childLabels s) ts

-------------------------
-- Set Constraints
-------------------------

maxInt = maxBound :: Int

mkLF :: String -> LinFunc String Int
mkLF s = linCombination [(1,s)]

mkLF2 :: String -> String -> LinFunc String Int
mkLF2 s1 s2 = linCombination [(1,s1), (1,s2)]

-- minimise total time
totalTime :: Map String Action -> LinFunc String Int
totalTime labelSet = let l = maximum $ Map.keys labelSet
                      in mkLF $ l ++ "_end"

-- calculate durations
durations :: Map String Action -> [(LinFunc String Int, Time)]
durations labelSet = [(mkLF $ l ++ "_duration", dur)
                        | (l,a) <- Map.toList labelSet
                        , let dur = timeAction a]
                                                     
-- end = start + duration
durConstraints :: Map String Action -> [(LinFunc String Int, LinFunc String Int)]
durConstraints labelSet = [(lhs, rhs) 
    | l <- Map.keys labelSet
    , let lhs = mkLF2 (l ++ "_start") (l ++ "_duration")
    , let rhs = mkLF $ l ++ "_end"]

-- end and start must be >= 0
allGeq :: Map String Action -> [(LinFunc String Int, Int)]
allGeq labelSet = let starts = f "_start"
                      ends   = f "_end"
                      f s    = [(mkLF $ l ++ s, 0) | l <- Map.keys labelSet]
                   in starts ++ ends

-- duration will always be positive, therefore
-- durConstraints and allGeq ensure end >= start

-- action can't start until child actions have ended
seqConstraints :: Recipe -> [(LinFunc String Int, LinFunc String Int)]
seqConstraints r = let labelSet  = mkLabelSet r
                       labelTree = fmap fst (mkLabelTree r)
                    in [(mkLF $ l ++ "_start", mkLF $ c ++ "_end")
                        | l <- Map.keys labelSet
                        , c <- childLabels l labelTree]

setStationNumbers :: Env -> [(LinFunc String Int, Int)]
setStationNumbers env = let names = map stName (eStations env)
                            xs = zip names [1..length names]
                         in map (\(s,i) -> (mkLF s, i)) xs

compatStations :: Env -> Recipe -> [StName]
compatStations env r = let xs = map (applyConstrF r) (eStations env)
                           applyConstrF r st = (stName st, (stConstrF st) r)
                           ys = filter (isJust . snd) xs
                        in map fst ys

-- Need to map OR
stationConstr :: Env -> Recipe -> [(LinFunc String Int, LinFunc String Int)]
stationConstr env r =
    let tree = fmap (\(l,r) ->
            (l, compatStations env r)) (mkLabelTreeR r)
     in [(mkLF l, mkLF st) | (l, sts) <- flatten tree
                           , st <- sts]

-- var i = s ++ show i -- y1, y2 etc.

-- or :: [(String, Int)] -> State Int [(LinFunc String Int, Int)]
-- or xs = 