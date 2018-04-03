{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Recipe.Printer where

import Recipe.Recipe
import Data.Tree
import Data.Tree.Pretty
import Control.Monad.Trans.State
import Recipe.Scheduler
import Recipe.Properties
import Recipe.Kitchen
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Diagrams.Prelude hiding (Time)
import Diagrams.Backend.SVG.CmdLine

-------------------------------------
-- Steps
-------------------------------------

type Step = (Label, String)

steps :: Recipe -> Tree Step
steps = steps' . labelRecipeA
    where
        steps' (Node (l,a) ts) = Node (l,s) ts'
            where
                ts' = map steps' ts
                s = toString a
                toString (GetIngredient s) = "Get " ++ s
                toString (Heat) = "Heat (" ++ show l' ++ ")"
                    where l' = head $ map extractLabel ts
                toString (HeatAt t) = "Heat (" ++ show l' ++ ") at " ++ show t
                    where l' = head $ map extractLabel ts
                toString Wait = "Wait"
                toString (Combine s) = s ++ " (" ++ show l' ++ ") and ("
                    ++ show l'' ++ ")"
                    where
                        l' = (map extractLabel ts) !! 0
                        l'' = (map extractLabel ts) !! 1
                toString (Conditional a c) = toString a ++ condToString c
                    where
                        condToString (CondOpt) = " (optional)"
                        condToString (CondTemp t) = " until temperature " ++ show t
                        condToString (CondTime t) = " for " ++ show t
                toString (Transaction a) = "Immediately " ++ toString a
                toString (Measure m) = "Measure " ++ show m ++ case m of
                    Count _ -> show l
                    _        -> " of " ++ show l
                    where l' = head $ map extractLabel ts
                extractLabel (Node (l,_) _) = l

ppSteps :: Recipe -> IO ()
ppSteps = ppSteps' . steps
    where
        ppSteps' (Node (l,s) ts) =
            mapM_ ppSteps' ts
            >> putStrLn (show l ++ ") " ++ s)

-------------------------------------
-- Tree
-------------------------------------

ppTree :: Show a => Tree a -> IO ()
ppTree = putStrLn . drawVerticalTree . fmap show

ppIngredients :: Recipe -> IO ()
ppIngredients = mapM_ putStrLn . ingredients

drawDiag :: Recipe -> Diagram B
drawDiag r =
    let stepTree = steps r
        lTree = fmap fst stepTree
        treeDiag = drawDiag' lTree
        instructions = concat $ flatten $ fmap (\(l,s) ->
            show l ++ ") " ++ s ++ "\n") stepTree
        stepsDiag = text instructions # fontSizeL 0.5
            <> roundedRect 10 20 0.3 # fc white 
     in treeDiag ||| stepsDiag
    where
        drawDiag' (Node l []) = drawAction l
        drawDiag' (Node l ts) =
            let na = drawAction l
                subTrees = map drawDiag' ts
                nts = foldr1 (|||) subTrees
                diag = na === nts # center
             in connectNodes l ts diag

connectNodes :: Label -> [Tree Label] -> Diagram B -> Diagram B
connectNodes _ [] d = d
connectNodes l (Node l' _ : ts) d =
    connectOutside l l' (connectNodes l ts d)

drawAction :: Label -> Diagram B
drawAction l = text (show l) # fontSizeL 0.7
    <> circle 1 # pad 2 # fc white # named l

-------------------------------------
-- Properties
-------------------------------------

ppIngredientsQ :: Recipe -> IO ()
ppIngredientsQ = mapM_ ppQuantIng . ingredientsQ
    where
        ppQuantIng (s,m) = putStrLn $
            s ++ ": " ++ show m

-------------------------------------
-- Schedule
-------------------------------------

printSchedule :: Schedule -> Map Label String -> Map Label Recipe -> IO ()
printSchedule sch = printSchedule' (Map.toList sch)
    where
        printSchedule' [] _ _ = return ()
        printSchedule' ((name, stack) : xs) sMap rMap = do
            putStrLn $ name ++ ":"
            evalStateT (printStack stack sMap rMap) (Time 0)
            printSchedule' xs sMap rMap

printStack :: Stack -> Map Label String -> Map Label Recipe -> StateT Time IO ()
printStack [] _ _ = return ()
printStack (Active l : xs) sMap rMap = do
    printStack xs sMap rMap

    let s = fromJust $ Map.lookup l sMap
    startT <- get
    let step = (show startT) ++ ": " ++ show l ++ ") " ++ s
    liftIO $ putStrLn step

    let (Node a _) = fromJust $ Map.lookup l rMap
    put (startT + timeAction a)

printStack (Idle t : xs) sMap rMap = do
    printStack xs sMap rMap

    let s = "Idle: " ++ show t
    startT <- get
    let step = (show startT) ++ ": " ++ s
    liftIO $ putStrLn step

    put (startT + t)

scheduleAndPrint :: Recipe -> Env -> IO ()
scheduleAndPrint r env =
    let sch = scheduleRecipe r env
        sMap = (Map.fromList . flatten . steps) r
        rMap = mkLabelMap $ labelRecipeR r
     in printSchedule sch sMap rMap