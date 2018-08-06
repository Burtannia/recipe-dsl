import           Recipe.Demo
import           Recipe.QS
import           Recipe.Simulator

{-|
Simple test for CI purposes, runs QuickCheck
tests and then an example simulation.
-}

-- |Throws an error if tests fail in order
-- to cause a CI build to fail.
main :: IO ()
main = do

    -- tests
    b <- runTests
    if b then
        putStrLn "Tests Successful"
    else
        error "Tests Failed"

    -- test simulation
    putStrLn "Running test simulation..."
    simulate jalfreziWithRice env 1000 False
