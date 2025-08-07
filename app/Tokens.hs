module Tokens(parseTokens) where
import Types
import Data.Char (isDigit)
import Data.Foldable (Foldable(foldl'))

parseTokens :: String -> Either String [Token]
parseTokens str = do
    let filteredStr = removeWhitespace str
    tokens <- parseTokensUnvalidated filteredStr ("", [])
    tokensWithExpressions <- createExpressions tokens
    validate $ addImplicitMultiplication tokensWithExpressions

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
    | hasParens = Left "Mismatched parentheses or invalid expression"
    | otherwise= Right tokens
    where
        hasParens = any (`elem` [TokenOpen, TokenClose]) tokens

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
    | otherwise = Left $ "Unrecognised character '" ++ [char] ++ "'"    where
        addNumberToken = if null str
            then tokens
            else TokenNumber (read str) : tokens

parseCharacterForString :: Char -> String -> Either String String
parseCharacterForString char str
    | isDigit char || char == '.' = Right $ char : str
    | char `elem` "+-*/^()=x" = Right ""
    | otherwise = Left $ "Unrecognised character '" ++ [char] ++ "'"

createExpressions :: [Token] -> Either String [Token]
createExpressions tokens
    | isInExpression || not (null currentExpression) = Left "Unmatched parentheses in expression"
    | not $ null err = Left err
    | otherwise = Right expressions
    where
        (expressions, currentExpression, isInExpression, err) = foldl' foldFn startingAcc tokens
        -- (expressions, current expression, is in expression, error) 
        startingAcc = ([], [], False, "")
        foldFn acc@(exp, cur, inExp, err) token
            | not $ null err = acc
            | not inExp && token == TokenOpen = (exp, [], True, "") 
            | not inExp && token == TokenClose = (exp, [], True, "Unmatched closing parenthesis") 
            | inExp && token == TokenOpen = (exp, [], True, "Nested parentheses are not allowed")
            | inExp && token == TokenClose = (exp ++ [TokenExpression cur], [], False, "") 
            | inExp && token == TokenEq = (exp, [], True, "Equal sign inside expression is not allowed") 
            | inExp = (exp, cur ++ [token], True, "")
            | otherwise = (exp ++ [token], [], False, "") 
        
addImplicitMultiplication :: [Token] -> [Token]
addImplicitMultiplication [] = []
addImplicitMultiplication t@[_] = t
addImplicitMultiplication (first:second:rest)
    | bothNonOperations = first:TokenMul:othersResult 
    | otherwise = first:othersResult 
    where
        isNonOperation t = case t of
            TokenNumber _ -> True
            TokenExpression _ -> True
            _ -> t == TokenVar
        bothNonOperations = isNonOperation first && isNonOperation second
        othersResult = addImplicitMultiplication $ second:rest