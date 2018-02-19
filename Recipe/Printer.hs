module Recipe.Printer where

import           Recipe.Recipe
import Data.Char
import Data.List
import Data.Tree
import Data.Tree.Pretty
import Control.Monad.State

-------------------------------------
-- PRINTING RECIPES
-------------------------------------

toTree :: Recipe -> Tree String
toTree (Ingredient s)    = Node s []
toTree (HeatAt t r)      = Node ("heat at " ++ show t) [toTree r]
toTree (Wait t r)        = Node ("wait for " ++ show t) [toTree r]
toTree (Combine r1 r2)   = Node "combine" [toTree r1, toTree r2]
toTree (Conditional c r) = Node ("condition " ++ show c)  [toTree r]
toTree (Transaction r)   = let Node s xs = toTree r
    in Node (s ++ " (T)") xs
toTree (Measure m r)     = let Node s xs = toTree r
    in Node (s ++ " - " ++ show m) xs

printRecipe :: Recipe -> IO ()
printRecipe = putStrLn . drawVerticalTree . toTree

{-
f:
Ingredient s = s
HeatAt t r = Heat (labelOf r) at t
Wait t r = Wait for t
Combine r1 r2 = Mix (labelOf r1) with (labelOf r2)
Conditional c r = (f r) (optional) / for time / until it reaches temp
Transaction r = Perform the following then immediately (f r):
                    (f child)
                    (f child)...
Measure m r = Measure m of (labelOf r)

labelOf r should return s for (Ingredient s)
-}

printSteps :: Recipe -> IO ()
printSteps r = mapM_ putStrLn (toString r)

toString :: Recipe -> [String]
toString r = reverse $ evalState (toString' r) 1
    where
        toString' :: Recipe -> State Int [String]
        toString' r = case r of
            Ingredient s -> return []

            HeatAt t r' -> do
                chStrs <- toString' r'
                chLab <- getChildLabel r'
                l <- get
                put (l + 1)
                let parStr = show l ++ ") Heat (" ++ chLab ++ ") at " ++ show t
                return $ parStr : chStrs

            Wait t r' -> do
                chStrs <- toString' r'
                l <- get
                put (l + 1)
                let parStr = show l ++ ") Wait for " ++ show t
                return $ parStr : chStrs

            Combine r1 r2 -> do
                chStrs1 <- toString' r1
                chLab1 <- getChildLabel r1

                chStrs2 <- toString' r2
                chLab2 <- getChildLabel r2

                l <- get
                put (l + 1)
                let parStr = show l ++ ") Combine (" ++ chLab1 ++ ") with (" ++ chLab2 ++ ")"
                return $ parStr : (chStrs2 ++ chStrs1)

            Conditional c r' -> do
                chStrs <- toString' r'

                let condSuff = case c of
                        CondTime t -> " for " ++ show t
                        CondTemp t -> " until " ++ show t
                        CondOpt -> " (optional)"

                case chStrs of
                    [] -> return []
                    (x:xs) -> do
                        let parStr = x ++ condSuff
                        return $ parStr : xs

            Transaction r' -> do
                chStrs <- toString' r'
                let numCs = numChildRecipes r'

                case chStrs of
                    [] -> return []
                    (x:xs) -> do
                        let indented = indent $ x : take numCs xs
                        return $ indented ++ drop numCs xs
                
            Measure m r' -> do
                chStrs <- toString' r'
                chLab <- getChildLabel r'

                l <- get
                put (l + 1)

                let parStr = show l ++ ") Measure " ++ show m ++ " of (" ++ chLab ++ ")"
                return $ parStr : chStrs
            where
                indent = map (\s -> '\t' : s)
                getChildLabel :: Recipe -> State Int String
                getChildLabel (Ingredient s) = return s
                getChildLabel _ = get >>= (\i -> return $ show (i - 1))

numChildRecipes :: Recipe -> Int
numChildRecipes = (length . childRecipes)

childRecipes :: Recipe -> [Recipe]
childRecipes r = filter notIng rs
    where
        rs = case r of
            Ingredient s     -> []
            HeatAt _ r'      -> [r']
            Wait _ r'        -> [r']
            Combine r1 r2    -> [r1, r2]
            Conditional _ r' -> childRecipes r'
            Transaction r'   -> childRecipes r'
            Measure _ r'     -> [r']
        notIng (Ingredient _) = False
        notIng _              = True

countAll :: Recipe -> Int
countAll r = case r of
    Ingredient _     -> 0
    HeatAt _ r'      -> 1 + countAll r'
    Wait _ r'        -> 1 + countAll r'
    Combine r1 r2    -> 2 + countAll r1 + countAll r2
    Conditional _ r' -> 1 + countAll r'
    Transaction r'   -> countAll r'
    Measure _ r'     -> countAll r'

-------------------------------------
-- PRINTING INGREDIENTS
-------------------------------------

-- Print the list of ingredients in a recipe
printIngredients :: Recipe -> IO ()
printIngredients r = mapM_ putStrLn (getIngredients r)

-------------------------------------
-- PRICE ... this probably should be somewhere else
-------------------------------------

type Price = Float
type PricedItem = (String, Price)
type PriceList = [PricedItem]

class Priced a where
    findCost :: a -> PriceList -> Price

instance Priced Recipe where
    findCost (Ingredient s) ps =
        case prices of
            [] -> 0
            _  -> head prices
        where prices = [p | (x, p) <- ps, x == s]
    findCost (HeatAt _ r) ps      = findCost r ps
    findCost (Combine r1 r2) ps   = findCost r1 ps + findCost r2 ps
    findCost (Wait _ r) ps        = findCost r ps
    findCost (Conditional _ r) ps = findCost r ps
    findCost (Transaction r) ps   = findCost r ps
    findCost (Measure _ r) ps     = findCost r ps

testList :: PriceList
testList = [ ("milk", 1.00)
           , ("teabag", 6.70)
           ]

-- Need to amend this once we have the measurement constructor
-- We don't need a full pack of teabags to make a cup of tea