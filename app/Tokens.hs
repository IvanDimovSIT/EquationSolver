module Tokens(parseTokens) where
import Types
import Data.Char (isDigit)


parseTokens :: String -> Either String [Token]
parseTokens str = do
    let filteredStr = removeWhitespace str
    tokens <- parseTokensUnvalidated filteredStr ("", [])
    validate tokens

removeWhitespace :: String -> String
removeWhitespace = filter (not.(`elem` " \t\n"))

parseTokensUnvalidated :: String -> (String, [Token]) -> Either String [Token]
parseTokensUnvalidated [] (str, tokens)
    | null str = Right tokens
    | otherwise = Right $ TokenNumber (read str) : tokens
parseTokensUnvalidated str acc = do
    result <- foldr foldFn (Right acc) str
    parseTokensUnvalidated "" result
    where
        foldFn char acc = case acc of
            Left acc -> Left acc
            Right acc -> parseCharacter char acc


validate :: [Token] -> Either String [Token]
validate [] = Left "No tokens generated"
validate tokens 
    | any isPar tokens = Left "Parenthesis not supported yet"
    | otherwise = Right tokens
    where
        isPar token = case token of
            TokenOpen -> True
            TokenClose -> True
            _ -> False

    
parseCharacter :: Char -> (String, [Token]) -> Either String (String, [Token])
parseCharacter char (str, tokens) = do
        newStr <- parseCharacterForString char str
        newTokens <- parseCharacterForToken char str tokens
        Right (newStr, newTokens)

parseCharacterForToken :: Char -> String -> [Token] -> Either String [Token]
parseCharacterForToken char str tokens
    | isDigit char || char == '.' = Right tokens
    | char == '+' = Right $ TokenPlus : addNumberToken
    | char == '-' = Right $ TokenMinus : addNumberToken
    | char == '*' = Right $ TokenMul : addNumberToken
    | char == '/' = Right $ TokenDiv : addNumberToken
    | char == '^' = Right $ TokenPow : addNumberToken
    | char == '=' = Right $ TokenEq : addNumberToken
    | char == '(' = Right $ TokenOpen : addNumberToken
    | char == ')' = Right $ TokenClose : addNumberToken
    | char == 'x' = Right $ TokenVar : addNumberToken
    | otherwise = Left $ "unrecognised character '" ++ [char] ++ "'"    where
        addNumberToken = if null str
            then tokens
            else TokenNumber (read str) : tokens

parseCharacterForString :: Char -> String -> Either String String
parseCharacterForString char str
    | isDigit char || char == '.' = Right $ char : str
    | char `elem` "+-*/^()=x" = Right ""
    | otherwise = Left $ "unrecognised character '" ++ [char] ++ "'"
