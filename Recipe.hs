module Recipe where

-- for CI purposes
milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"

cupOfTea :: Recipe
cupOfTea = milk >< ((teabag >< heat 100 water) >>> wait 5)

main :: IO ()
main = print cupOfTea

-------------------------------------

type Time = Int
type Temperature = Int

data Recipe = Ingredient String
            | Heat Temperature Recipe
            | Combine Recipe Recipe
            | Wait Time
            | Sequence Recipe Recipe
            deriving (Show)

heat :: Temperature -> Recipe -> Recipe
heat = Heat

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

wait :: Time -> Recipe
wait = Wait

-- r1 then r2
(>>>) :: Recipe -> Recipe -> Recipe
(>>>) = Sequence

-------------------------------------

-- More complex definitions

-- data Quantity = Measurement Int | Fraction Int Int
--
-- instance Show Quantity where
--         show (Measurement i) = show i
--         show (Fraction n d) = "take " ++ show n ++ "//" ++ show d ++ " of the recipe"
--
-- data Position = Beside | Above | Wrap
--
-- instance Show Position where
-- 	show Beside = "and place it next to"
-- 	show Above = "and place it on top of"
-- 	show Wrap = "and wrap it in"

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Prepare Recipe -- cut, slice, peel etc.
--             | Wait Time
--             | Assemble Position Recipe Recipe
--             | After Recipe Recipe
--             | Measure Quantity Recipe
--             deriving (Show)

-- prepare :: Recipe -> Recipe
-- prepare = Prepare
--
-- measure :: Quantity -> Recipe -> Recipe
-- measure = Measure
--
-- -- assemble vertically (r1 above r2)
-- (^^^) :: Recipe -> Recipe -> Recipe
-- r1 ^^^ r2 = Assemble Above r1 r2
--
-- -- assemble horizontally
-- (~~~) :: Recipe -> Recipe -> Recipe
-- r1 ~~~ r2 = Assemble Beside r1 r2
--
-- -- wrap r2 in r1
-- (@@@) :: Recipe -> Recipe -> Recipe
-- r1 @@@ r2 = Assemble Wrap r1 r2

-- Must consider how to deal with splitting a recipe, at the moment it would be something like this:
-- (orangeZest >< (measure (Fraction 1 2) cakeMix)) ^^^ (lemonZest >< (measure (Fraction 1 2) cakeMix)