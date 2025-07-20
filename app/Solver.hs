module Solver where

import Types

solve :: [Token] -> Either String Double
solve [] = Left "Empty input"
solve tokens = do
    (left, right) <- splitEquation tokens
    let (leftTerms, rightTerms) = (toTerms left, toTerms right)
    leftNorm <- normalise $ Right leftTerms
    rightNorm <- normalise $ Right rightTerms

    Left $ show leftNorm


splitEquation :: [Token] -> Either String ([Token], [Token])
splitEquation tokens
    | null left || null right = Left "invalid equation"
    | otherwise = Right splitEq
    where
        splitEq@(left, right) = (takeWhile isNotEqual tokens, tailOrEmpty $ dropWhile isNotEqual tokens)
        isNotEqual token = case token of
            TokenEq -> False
            _ -> True


tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_:other) = other


normalise :: Either String [Token] -> Either String [Token]
normalise = validateRemaining.applyMulAndDiv.applyPow.applyParenthesis


toTerms :: [Token] -> [Token]
toTerms [] = []
toTerms ((TokenNumber n):rest) = TokenTerm (Term (n, 0)):toTerms rest
toTerms (TokenVar:rest) = TokenTerm (Term (1, 1)):toTerms rest
toTerms (t:rest) = t:toTerms rest


applyParenthesis :: Either String [Token] -> Either String [Token]
applyParenthesis (Right []) = Right []
applyParenthesis (Left err) = Left err
applyParenthesis (Right (TokenOpen:t1@(TokenTerm _):operation:t2@(TokenTerm _):TokenClose:TokenPow:p@(TokenTerm (Term(powCoef, pow))):rest))
    | powCoef == 2 && pow == 0 = case operation of
        TokenPlus -> do
            result <- applyParenthesis $ Right rest
            Right $ t1:TokenPow:p:TokenPlus:p:TokenMul:t1:TokenMul:t2:TokenPlus:t2:TokenPow:p:result
        TokenMinus -> do
            result <- applyParenthesis $ Right rest
            Right $ t1:TokenPow:p:TokenMinus:p:TokenMul:t1:TokenMul:t2:TokenPlus:t2:TokenPow:p:result
        _ -> Left "unrecognised operation in parenthesis"
    | otherwise = Left "structure not supported"
applyParenthesis (Right (t:rest)) = case t of
    TokenOpen -> Left "unrecognised parenthesis structure"
    TokenClose -> Left "unrecognised parenthesis structure"
    _ -> do
        result <- applyParenthesis $ Right rest
        Right $ t:result


applyPow :: Either String [Token] -> Either String [Token]
applyPow (Right []) = Right []
applyPow (Left err) = Left err
applyPow (Right ((TokenTerm (Term(coef1, pow1))):TokenPow:(TokenTerm (Term(coef2, pow2))):rest))
    | pow2 == 0 = do
        result <- applyPow $ Right rest
        Right $ TokenTerm (Term (coef1**coef2,pow1*round coef2)):result
    | otherwise = Left "Power of X not supported"
applyPow (Right (other:rest)) = do
    result <- applyPow $ Right rest
    Right $ other:result


applyMulAndDiv :: Either String [Token] -> Either String [Token]
applyMulAndDiv (Right []) = Right []
applyMulAndDiv (Left err) = Left err
applyMulAndDiv (Right ((TokenTerm (Term(coef1,pow1))):TokenMul:(TokenTerm (Term(coef2,pow2))):rest)) = do
    let product = TokenTerm (Term (coef1 * coef2, pow1 + pow2))
    applyMulAndDiv $ Right $ product:rest
applyMulAndDiv (Right ((TokenTerm (Term(coef1,pow1))):TokenDiv:(TokenTerm (Term(coef2,pow2))):rest))
    | coef2 == 0 = Left "division by zero"
    | otherwise = do
        let divisionResult = TokenTerm (Term (coef1 / coef2, pow1 - pow2))
        applyMulAndDiv $ Right $ divisionResult:rest
applyMulAndDiv (Right (t:rest)) = do
    result <- applyMulAndDiv $ Right rest
    Right $ t:result


validateRemaining :: Either String [Token] -> Either String [Token]
validateRemaining empty@(Right []) = empty
validateRemaining err@(Left _) = err
validateRemaining (Right (t:rest))
    | isValidToken t = do
        result <- validateRemaining $ Right rest
        Right $ t:result
    | otherwise = Left $ "Equation has left over token '" ++ show t ++ "'"
    where
        isValidToken token = case token of
            TokenTerm _ -> True
            TokenPlus -> True
            TokenMinus -> True
            _ -> False