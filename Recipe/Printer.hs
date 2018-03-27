module Recipe.Printer where

import Recipe.Recipe
import Data.Tree
import Data.Tree.Pretty
import Control.Monad.Trans.State

type Step = (Label, String)

steps :: Recipe -> Tree Step
steps = steps' . labelRecipe
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
                    Number _ -> show l
                    _        -> " of " ++ show l
                    where l' = head $ map extractLabel ts
                extractLabel (Node (l,_) _) = l

ppSteps :: Recipe -> IO ()
ppSteps = ppSteps' . steps
    where
        ppSteps' (Node (l,s) ts) =
            mapM_ ppSteps' ts
            >> putStrLn (show l ++ ") " ++ s)

ppTree :: Show a => Tree a -> IO ()
ppTree = putStrLn . drawVerticalTree . fmap show

ppIngredients :: Recipe -> IO ()
ppIngredients = mapM_ putStrLn . ingredients

ppIngredientsQ :: Recipe -> IO ()
ppIngredientsQ = mapM_ ppQuantIng . ingredientsQ
    where
        ppQuantIng (s,m) = putStrLn $
            s ++ ": " ++ show m