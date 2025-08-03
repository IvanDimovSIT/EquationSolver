module Main where
import Tokens (parseTokens)
import Solver (solve)

main :: IO ()
main = do
    putStrLn "Enter an equation:"
    str <- getLine
    let result = solveEquation str
    case result of
        Left err -> putStrLn $ "Error:" ++ err
        Right solutions -> printSolutions solutions

printSolutions :: [Double] -> IO ()
printSolutions [] = putStrLn "No solutions in R"
printSolutions [x] = putStrLn $ "x=" ++ show x
printSolutions [x1, x2] = putStrLn $ "x1=" ++ show x1 ++ ", x2=" ++ show x2
printSolutions solutions = print solutions

solveEquation :: String -> Either String [Double]
solveEquation str = do
    tokens <- parseTokens str
    solve tokens
