module Recipe.QS where

import Recipe.Recipe
import QuickSpec
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.Tree

-------------------------------------
-- Arbitrary Instances
-------------------------------------

instance Arbitrary Action where
    arbitrary = oneof
        [ genIng
        , return Heat
        , liftM HeatAt genTemp
        , return Wait
        , liftM Combine genMethod
        , liftM2 Conditional arbitrary arbitrary
        , liftM Transaction arbitrary
        , liftM Measure arbitrary
        ]

genIng :: Gen Action
genIng = liftM (GetIngredient . show) ((choose (1, 100)) :: Gen Int)

genTemp :: Gen Int
genTemp = choose (100, 240)

genMethod :: Gen String
genMethod = elements
    [ "mix"
    , "spread"
    , "wrap" ]

instance Arbitrary Condition where
    arbitrary = oneof
        [ singleCond
        , liftM2 AND singleCond singleCond
        , liftM2 OR singleCond singleCond ]
        
singleCond :: Gen Condition
singleCond = oneof
    [ return CondOpt
    , liftM CondTime genTime
    , liftM CondTemp genTemp ]

genTime :: Gen Time
genTime = liftM Time (choose (1, 600))

instance Arbitrary Measurement where
    arbitrary = oneof
        [ liftM Number (elements [1..10])
        , liftM Grams (elements [10,20..1000])
        , liftM Milliletres (elements [10,20..1000]) ]

genRecipe :: Gen Recipe
genRecipe = sized $ \n ->
    let bin = resize (n `div` 2) (vectorOf 2 arbitrary)
        un  = resize (n-1) (vectorOf 1 arbitrary) in
    oneof $
        [ liftM2 Node arbitrary (return [])
        , liftM2 Node arbitrary un
        , liftM2 Node arbitrary bin ]

-------------------------------------
-- QuickSpec Stuff
-------------------------------------

qsRecipe = quickSpec
    [ con "ingredient" (ingredient :: String -> Recipe)
    , con "heat" (heat :: Recipe -> Recipe)
    , con "heatAt" (heatAt :: Int -> Recipe -> Recipe)
    , con "wait" (wait :: Recipe -> Recipe)
    , con "combine" (combine :: String -> Recipe -> Recipe -> Recipe)
    , con "addCondition" (addCondition :: Condition -> Recipe -> Recipe)
    , con ".&&" ((.&&) :: Condition -> Condition -> Condition)
    , con ".||" ((.||) :: Condition -> Condition -> Condition)
    , con "transaction" (transaction :: Recipe -> Recipe)
    , con "measure" (measure :: Measurement -> Recipe -> Recipe)

    , monoType (Proxy :: Proxy Recipe)
    , monoType (Proxy :: Proxy Measurement)
    , monoType (Proxy :: Proxy Condition)
    ]