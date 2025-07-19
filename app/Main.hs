module Main where
import Tokens (parseTokens)
import Solver (solve)

main :: IO ()
main = do
    str <- getLine
    let tokens = parseTokens str
    case tokens of
        Left err -> putStrLn $ "Error: " ++ err
        Right t -> do
            let result = solve t
            print result
    return ()
