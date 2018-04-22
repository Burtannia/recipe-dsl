import          Recipe.Demo
import          Recipe.Simulator
import          Recipe.QS

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
    simulate jalfreziWithRice env 1000 False