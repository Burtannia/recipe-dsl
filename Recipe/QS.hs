module Recipe.QS where

import Recipe.Recipe
import QuickSpec
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

instance Arbitrary Recipe where
    arbitrary = sized $ \n ->
        let bin = resize (n `div` 2) arbitrary
            un = resize (n-1) arbitrary in
        oneof $
            genIng :
            [liftM2 HeatAt genTemp un | n > 0] ++
            [liftM2 Wait genTime un | n > 0] ++
            [liftM2 Combine bin bin | n > 0] ++
            [liftM2 Conditional genCond un | n > 0] ++
            [liftM Transaction un | n > 0] ++
            [liftM2 Measure genMeasure un | n > 0]

genIng :: Gen Recipe
genIng = fmap (Ingredient . show) ((choose (1, 100)) :: Gen Int)

genTemp :: Gen Temperature
genTemp = oneof $ 
    [ fmap Deg (choose (100, 240))
    , elements [Low, Medium, High] ]

genTime :: Gen Time
genTime = choose (1, 600)

genCond :: Gen Condition
genCond = oneof 
    [ return CondOpt
    , fmap CondTime genTime
    , fmap CondTemp genTemp ]

genMeasure :: Gen Measurement
genMeasure = elements [100,200..1000]

genRecipe :: IO Recipe
genRecipe = generate arbitrary

{-
sig = 
	[ withMaxTermSize 7
	, con "Ingredient" (Ingredient :: String -> Recipe)
	, con "HeatAt" (HeatAt :: Temperature -> Recipe -> Recipe)
	, con "Wait" (Wait :: Time -> Recipe -> Recipe)
	, con "Combine" (Combine :: Recipe -> Recipe -> Recipe)
	, con "Conditional" (Conditional :: Condition -> Recipe -> Recipe)
	, con "Transaction" (Transaction :: Recipe -> Recipe)
	, con "Measure" (Measure :: Measurement -> Recipe -> Recipe)
	]

qsRecipe :: IO Signature
qsRecipe = quickSpec sig -}