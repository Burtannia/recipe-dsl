import Recipe.Recipe
import Recipe.QS
import Recipe.Kitchen
--import Recipe.Properties
import Recipe.Scheduler
import Test.QuickCheck
--import Recipe.Demo
import Recipe.Printer

main :: IO ()
main = quickCheck prop_combine_comm
--main = scheduleAndPrint cupOfTea env
--main = qsRecipe
--main = quickCheck (\s r1 r2 -> combine s r1 r2 == combine s r2 r1)