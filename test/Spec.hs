module Main (main) where

import Test.HUnit
import TokensTests
import SolverTests
import IntegrationTests
import Data.Time.Clock

main :: IO ()
main = do
    start <- getCurrentTime
    _ <- runTestTT $ TestList [tokensTests, solverTests, integrationTests]
    end <- getCurrentTime
    putStrLn $ "Tests took: " ++ show (diffUTCTime end start)