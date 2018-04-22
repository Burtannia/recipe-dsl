{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
This module contains functions for the printing
of recipes and recipe schedules.
-}

module Recipe.Printer where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import           Data.Char                 (toUpper)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust)
import           Data.Tree
import           Data.Tree.Pretty
import           Recipe.Kitchen
import           Recipe.Recipe
import           Recipe.Scheduler

-------------------------------------
-- Steps
-------------------------------------

-- |Given a list of trees, extract either their labels
-- or, if a tree's root node is an ingredient, extract
-- the ingredient name.
extractLabel :: [Tree (Label, Action)] -> [String]
extractLabel [t] = [extractLabel' t]
extractLabel ts  = map extractLabel' ts

-- |Helper function for 'extractLabel'. If a node contains
-- a label and ingredient, return the name of the ingredient,
-- else return the label as a String.
extractLabel' :: Tree (Label, Action) -> String
extractLabel' (Node (_, GetIngredient s) _) = s
extractLabel' (Node (l, _) _)               = show l

-- |Step is a pair of a label (step number) and the instructions for that step.
type Step = (Label, String)

-- |Translates a recipe into a tree of steps.
steps :: Recipe -> Tree Step
steps = steps' . labelRecipeA
    where
        steps' (Node (l,a) ts) = Node (l,s) ts'
            where
                ts' = map steps' ts
                s = toString a
                toString (GetIngredient s) = "Get " ++ s
                toString (Heat) = "Heat (" ++ l' ++ ")"
                    where l' = head $ extractLabel ts
                toString (HeatAt t) = "Heat (" ++ l' ++ ") at " ++ show t
                    where l' = head $ extractLabel ts
                toString Wait = "Wait"
                toString (Combine (c:s)) = toUpper c : s ++ " (" ++ l' ++ ") and ("
                    ++ l'' ++ ")"
                    where
                        l' = extractLabel ts !! 0
                        l'' = extractLabel ts !! 1
                toString (Conditional a c) = toString a ++ condToString c
                    where
                        condToString (CondOpt _) = " (optional)"
                        condToString (CondTemp t) = " until temperature " ++ show t
                        condToString (CondTime t) = " for " ++ show t
                toString (Transaction a) = "Immediately " ++ toString a
                toString (Measure m) = "Measure " ++ show m ++ " of " ++ l'
                    where l' = head $ extractLabel ts

-- |Prints one step per line in the following style.
-- 1) ...
-- 2) ...
ppSteps :: Recipe -> IO ()
ppSteps = ppSteps' . steps
    where
        ppSteps' (Node (l,s) ts) =
            mapM_ ppSteps' ts
            >> putStrLn (show l ++ ") " ++ s)

-------------------------------------
-- Tree
-------------------------------------

-- |Prints the recipe as an ASCII tree.
ppTree :: Show a => Tree a -> IO ()
ppTree = putStrLn . drawVerticalTree . fmap show

-- |Prints one ingredient name per line.
ppIngredients :: Recipe -> IO ()
ppIngredients = mapM_ putStrLn . ingredients

-------------------------------------
-- Properties
-------------------------------------

-- |Prints one ingredient name per line along with it's quantity.
ppIngredientsQ :: Recipe -> IO ()
ppIngredientsQ = mapM_ ppQuantIng . ingredientsQ
    where
        ppQuantIng (s,m) = putStrLn $
            s ++ ": " ++ show m

-------------------------------------
-- Schedule
-------------------------------------

-- |Print the given schedule using the given maps.
-- The first is a map of step numbers to their instructions,
-- the second is a map of labels to their recipes from the labelled tree.
printSchedule :: Schedule Label -> Map Label String -> Map Label Recipe -> IO ()
printSchedule sch = printSchedule' (Map.toList sch)
    where
        printSchedule' [] _ _ = return ()
        printSchedule' ((name, stack) : xs) sMap rMap = do
            putStrLn ""
            putStrLn $ name ++ ":"
            evalStateT (printStack stack sMap rMap) (Time 0)
            printSchedule' xs sMap rMap

-- |Print the given stack of actions.
-- The first is a map of step numbers to their instructions,
-- the second is a map of labels to their recipes from the labelled tree.
printStack :: Stack Label -> Map Label String -> Map Label Recipe -> StateT Time IO ()
printStack [] _ _ = return ()
printStack (Active l : xs) sMap rMap = do
    printStack xs sMap rMap

    let s = fromJust $ Map.lookup l sMap
    startT <- get
    let step = (show startT) ++ ": " ++ show l ++ ") " ++ s
    liftIO $ putStrLn step

    let t = duration l rMap
    put (startT + t)

printStack (Idle t : xs) sMap rMap = do
    printStack xs sMap rMap

    let s = "Idle: " ++ show t
    startT <- get
    let step = (show startT) ++ ": " ++ s
    liftIO $ putStrLn step

    put (startT + t)

-- |Schedule the given recipe in the given environemnt
-- and print the resulting schedule.
scheduleAndPrint :: Recipe -> Env -> IO ()
scheduleAndPrint r env =
    let sch = scheduleRecipe r env
        sMap = (Map.fromList . flatten . steps) r
        rMap = mkLabelMap $ labelRecipeR r
     in printSchedule sch sMap rMap
