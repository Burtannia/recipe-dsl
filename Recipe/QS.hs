{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Recipe.QS where

import Recipe.Recipe
import QuickSpec
import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)
import Data.Tree

-------------------------------------
-- Arbitrary Instances
-------------------------------------

instance {-# OVERLAPPING #-} Arbitrary Recipe where
    arbitrary = genRecipe

genRecipe :: Gen Recipe
genRecipe = sized $ \n ->
    let bin = resize (n `div` 2) genRecipe
        un  = resize (n-1) genRecipe
     in case n of
        1 -> genIng
        _ -> oneof [genUnRec un, genBinRec bin bin]

genIng :: Gen Recipe
genIng = liftM (ingredient . show) ((choose (1, 100)) :: Gen Int)

genUnRec :: Gen Recipe -> Gen Recipe
genUnRec un = oneof
    [ liftM heat un
    , liftM2 heatAt genTemp un
    , liftM wait un
    , liftM2 addCondition arbitrary un
    , liftM transaction un
    , liftM2 measure arbitrary un ]

genBinRec :: Gen Recipe -> Gen Recipe -> Gen Recipe
genBinRec r1 r2 = liftM3 combine genMethod r1 r2

-- instance Arbitrary Action where
--     arbitrary = oneof
--         [ genIng
--         , return Heat
--         , liftM HeatAt genTemp
--         , return Wait
--         , liftM Combine genMethod
--         , liftM2 Conditional arbitrary arbitrary
--         , liftM Transaction arbitrary
--         , liftM Measure arbitrary ]

-- genIng :: Gen Action
-- genIng = liftM (GetIngredient . show) ((choose (1, 100)) :: Gen Int)

-- genUnAct :: Gen Action
-- genUnAct = oneof
--     [ return Heat
--     , liftM HeatAt genTemp
--     , return Wait
--     , liftM2 Conditional arbitrary arbitrary
--     , liftM Transaction arbitrary
--     , liftM Measure arbitrary ]

-- genBinAct :: Gen Action
-- genBinAct = liftM Combine genMethod

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
    , liftM CondTime arbitrary
    , liftM CondTemp genTemp ]

instance Arbitrary Time where
    arbitrary = liftM Time (choose (1, 600))

instance Arbitrary Measurement where
    arbitrary = oneof
        [ liftM Count (elements [1..10])
        , liftM Grams (elements [10,20..1000])
        , liftM Milliletres (elements [10,20..1000]) ]

-------------------------------------
-- QuickSpec Stuff
-------------------------------------

qsRecipe = quickSpec
    [ withMaxTermSize 5
    
    , con "ingredient" (ingredient :: String -> Recipe)
    , con "heat" (heat :: Recipe -> Recipe)
    , con "heatAt" (heatAt :: Int -> Recipe -> Recipe)
    , con "wait" (wait :: Recipe -> Recipe)
    , con "combine" (combine :: String -> Recipe -> Recipe -> Recipe)
    , con "addCondition" (addCondition :: Condition -> Recipe -> Recipe)
    , con ".&&" ((.&&) :: Condition -> Condition -> Condition)
    , con ".||" ((.||) :: Condition -> Condition -> Condition)
    , con "transaction" (transaction :: Recipe -> Recipe)
    , con "measure" (measure :: Measurement -> Recipe -> Recipe)

    -- , con "optional" (optional :: Recipe -> Recipe)
    -- , con "toTemp" (toTemp :: Int -> Recipe -> Recipe)
    -- , con "forTime" (forTime :: Time -> Recipe -> Recipe)
    -- , con "hours" (hours :: Int -> Time)
    -- , con "minutes" (minutes :: Int -> Time)

    , monoType (Proxy :: Proxy Recipe)
    , monoType (Proxy :: Proxy Measurement)
    , monoType (Proxy :: Proxy Condition)
    , monoType (Proxy :: Proxy Time) ]