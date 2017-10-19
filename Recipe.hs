module Recipe where

-- for CI purposes
main :: IO ()
main = putStrLn "Hello World"

{- Cup of Tea:
- Boil Water / Heat Water to 100 / Heat Water
- Teabag into teapot
- Pour ...ml water into teapot
- Soak Teabag for ...minutes
- Pour tea into mug
- Add Milk

---------------

- Heat Water
- Water `combine` Teabag
- Wait 5 minutes
- Step 2 `mix` Milk
-}

{- Beans on Toast:
Buttered Toast:
- Heat bread
- Toast `combine` Butter
- Heat Beans
- Buttered Toast `combine` beans
-}

--------------

type Time = Int
type Temperature = Int

--type Quantity = Int
-- But a quantity could not only be a measurement e.g. 100g it could also be a fraction of
-- what we have made i.e. 1/2 of your cake mix.
data Quantity = Measurement Int | Fraction Int Int

instance Show Quantity where
        show (Measurement i) = show i
        show (Fraction n d) = "take " ++ show n ++ "//" ++ show d ++ " of the recipe"

data Position = Beside | Above | Wrap
  
instance Show Position where
	show Beside = "and place it next to"
	show Above = "and place it on top of"
	show Wrap = "and wrap it in"

data Recipe = Ingredient String
            | Heat Temperature Recipe
            | Combine Recipe Recipe
            | Prepare Recipe -- cut, slice, peel etc.
            | Wait Time
            | Assemble Position Recipe Recipe
            | After Recipe Recipe
            | Measure Quantity Recipe
            deriving (Show)

-- Heat followed by Wait is cook at temp for time whereas Heat alone is just heat to temp
-- Boil water = Heat 100 water
-- Baked potato = Wait 90 $ Heat 200 potato

-- But what about when the action has to be continued for time e.g. stirring
-- this is to do with sequencing, you either combine the "wait" and "stirring"
-- or you "wait" `after` "stir"

heat :: Temperature -> Recipe -> Recipe
heat = Heat

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

prepare :: Recipe -> Recipe
prepare = Prepare

wait :: Time -> Recipe
wait = Wait

-- r1 after r2
after :: Recipe -> Recipe -> Recipe
after = After

measure :: Quantity -> Recipe -> Recipe
measure = Measure

-- assemble vertically (r1 above r2)
(^^^) :: Recipe -> Recipe -> Recipe
r1 ^^^ r2 = Assemble Above r1 r2

-- assemble horizontally
(~~~) :: Recipe -> Recipe -> Recipe
r1 ~~~ r2 = Assemble Beside r1 r2

-- wrap r2 in r1
(@@@) :: Recipe -> Recipe -> Recipe
r1 @@@ r2 = Assemble Wrap r1 r2

-- Must consider how to deal with splitting a recipe, at the moment it would be something like this:
-- (orangeZest >< (measure (Fraction 1 2) cakeMix)) ^^^ (lemonZest >< (measure (Fraction 1 2) cakeMix)

-------------------------------------

milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"

cupOfTea :: Recipe
cupOfTea = milk >< (wait 5 `after` (teabag >< heat 100 water))

butter, bread, beans :: Recipe
butter = Ingredient "butter"
bread = Ingredient "bread"
beans = Ingredient "beans"

beansOnToast :: Recipe
beansOnToast = heat 3 beans ^^^ (butter ^^^ heat 3 bread)

