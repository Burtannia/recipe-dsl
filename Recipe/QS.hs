{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Recipe.QS where

import Recipe.Recipe
import QuickSpec
import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)
import Data.Tree
import Recipe.Kitchen
import Data.List (sort)

-------------------------------------
-- Arbitrary Instances
-------------------------------------

instance {-# OVERLAPPING #-} Arbitrary Recipe where
    arbitrary = sized $ \n ->
        if n > 4 then
            resize 4 arbitrary
        else
            let un = resize (n-1) arbitrary
                bin = resize (n `div` 2) arbitrary
            in case n of
                    0 -> genIng
                    1 -> oneof
                        [genIng, genUnRec un]
                    _ -> oneof
                        [genIng, genUnRec un, genBinRec bin bin]

genIng :: Gen Recipe
genIng = liftM (ingredient . show) (choose (1, 100) :: Gen Int)

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

-------------------------------------
-- QuickSpec Stuff
-------------------------------------

qsRecipe = quickSpec
    [ withMaxTestSize 4
    
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

prop_and_id x =
    x .&& x == x

prop_or_id x =
    x .|| x == x

prop_and_comm x y =
    x .&& y == y .&& x

prop_or_comm x y =
    x .|| y == y .|| x

prop_min_hours x =
    minutes (hours x) == hours (minutes x)

prop_min_min x =
    hours x == minutes (minutes x)

prop_combine_comm s r1 r2 =
    combine s r1 r2 == combine s r2 r1

return []
runTests = $quickCheckAll