module Main where
import Tokens (parseTokens)
import Solver (solve)
import Types

main :: IO ()
main = do
    putStrLn "Enter an equation:"
    str <- getLine
    let result = solveEquation str
    case result of
        Left err -> putStrLn $ "Error:" ++ err
        Right solutions -> printSolutions solutions

solveEquation :: String -> Either String SolutionResult
solveEquation str = parseTokens str >>= solve

printSolutions :: SolutionResult -> IO ()
printSolutions NoRealSolutions = putStrLn "No solutions in R"
printSolutions (FiniteSolutions [x]) = putStrLn $ "x=" ++ show x
printSolutions (FiniteSolutions [x1, x2]) = putStrLn $ "x1=" ++ show x1 ++ ", x2=" ++ show x2
printSolutions (FiniteSolutions solutions) = print solutions
printSolutions InfiniteSolutions = putStrLn "Infinitely many solutions"
