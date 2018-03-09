import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Tree
import Data.Tree.Pretty
import Control.Monad.State

type Recipe = Tree Action

data Action = GetIngredient String
            | HeatAt Int
            | Wait
            | Combine String
            | Conditional Action Condition
            | Transaction Action
            -- | Measure Measurement Recipe
            deriving Show

data Condition = CondTemp Int | CondTime Int | CondOpt
    deriving Show

ingredient :: String -> Recipe
ingredient s = Node (GetIngredient s) []

heatAt :: Int -> Recipe -> Recipe
heatAt temp r = Node (HeatAt temp) [r]

wait :: Recipe -> Recipe
wait r = Node Wait [r]

combine :: String -> Recipe -> Recipe -> Recipe
combine s r1 r2 = Node (Combine s) [r1, r2]

addCondition :: Condition -> Recipe -> Recipe
addCondition cond (Node a ts) = Node (Conditional a cond) ts

transaction :: Recipe -> Recipe
transaction (Node a ts) = Node (Transaction a) ts

water, teabag, milk :: Recipe
water = ingredient "water"
teabag = ingredient "teabag"
milk = ingredient "milk"

cupOfTea :: Recipe
cupOfTea = wait5 $ combine "pour" boilingWater teabag
    where
        boilingWater = addCondition (CondTemp 100) (heatAt 100 water)
        wait5 = \r -> addCondition (CondTime 5) (wait r)

butter, bread :: Recipe
butter = ingredient "butter"
bread = ingredient "bread"

butteredToast :: Recipe
butteredToast = transaction $ combine "spread" butter toast
    where toast = addCondition (CondTime 3) (heatAt 600 bread)

type Label = Int

labelRecipe :: Recipe -> Tree (Label, Action)
labelRecipe r = evalState (labelRecipe' r) 1
    where
        labelRecipe' (Node a ts) = do
            ts' <- mapM labelRecipe' ts
            l <- get
            put (l + 1)
            return $ Node (l,a) ts'

ppTree :: Show a => Tree a -> IO ()
ppTree = putStrLn . drawVerticalTree . fmap show

steps :: Recipe -> Tree (Label, String)
steps = steps' . labelRecipe
    where
        steps' (Node (l,a) ts) = Node (l,s) ts'
            where
                ts' = map steps' ts
                s = toString a
                toString (GetIngredient s) = "Get " ++ s
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
                        condToString (CondTime t) = " for " ++ show t ++ " minutes"
                toString (Transaction a) = "Immediately " ++ toString a
                extractLabel (Node (l,_) _) = l

ppSteps :: Recipe -> IO ()
ppSteps = ppSteps' . steps
    where
        ppSteps' (Node (l,s) ts) =
            mapM_ ppSteps' ts
            >> putStrLn (show l ++ ") " ++ s)
