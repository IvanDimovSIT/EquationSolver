module Tokens(parseTokens) where
import Types
import Data.Char (isDigit)
import Data.Foldable (Foldable(foldr'))

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
    | not $ null err = Left err
    | closeCount /= 0 || not (null currentExpression) = Left "Unmatched parentheses in expression"
    | otherwise = createSubExpressions expressions
    where
        (expressions, currentExpression, closeCount, err) = foldr' foldFn startingAcc tokens
        -- (expressions, current expression, close count, error) 
        startingAcc = ([], [], 0, "")
        foldFn token acc@(exp, cur, close, err)
            | not $ null err = acc
            | close == 0 && token == TokenClose = (exp, [], close+1, "")
            | close == 0 && token == TokenOpen = (exp, [], 0, "Unmatched closing parenthesis")
            | close > 0 && token == TokenClose = (exp, token : cur, close+1, "")
            | close == 1 && token == TokenOpen = (TokenExpression cur : exp, [], close-1, "")
            | close > 1 && token == TokenOpen = (exp, token : cur, close-1, "")
            | close > 0 && token == TokenEq = (exp, [], close, "Equal sign inside expression is not allowed")
            | close > 0 = (exp, token : cur, close, "")
            | otherwise = (token : exp, [], 0, "")

createSubExpressions :: [Token] -> Either String [Token]
createSubExpressions = mapM mapFn
    where
        mapFn (TokenExpression ex) = TokenExpression <$> createExpressions ex
        mapFn token = Right token


addImplicitMultiplication :: [Token] -> [Token]
addImplicitMultiplication tokens = map addToSubExpressionsMapFn $ go tokens
    where
        addToSubExpressionsMapFn (TokenExpression ex) = TokenExpression 
            $ addImplicitMultiplication ex
        addToSubExpressionsMapFn t = t
        go [] = []
        go t@[_] = t
        go (first:second:rest)
            | bothNonOperations = first:TokenMul:othersResult
            | otherwise = first:othersResult
            where
                isNonOperation t = case t of
                    TokenNumber _ -> True
                    TokenExpression _ -> True
                    _ -> t == TokenVar
                bothNonOperations = isNonOperation first && isNonOperation second
                othersResult = addImplicitMultiplication $ second:rest