module Solver where

import Types

solve :: [Token] -> Either String Double
solve [] = Left "Empty input"
solve tokens = do
    (left, right) <- splitEquation tokens
    let (leftPow, rightPow) = (applyPow left, applyPow right)
    
    Left $ show leftPow


splitEquation :: [Token] -> Either String ([Token], [Token])
splitEquation tokens
    | null left || null right = Left "invalid equation"
    | otherwise = Right splitEq
    where
        splitEq@(left, right) = (takeWhile isNotEqual tokens, tail $ dropWhile isNotEqual tokens)
        isNotEqual token = case token of
            TokenEq -> False
            _ -> True

applyPow :: [Token] -> [Token]
applyPow [] = []
applyPow ((TokenNumber n1):TokenPow:(TokenNumber n2):rest) = TokenTerm (Term (n1**n2, 0)):applyPow rest
applyPow (TokenVar:TokenPow:(TokenNumber n):rest) = TokenTerm (Term (1.0, round n)):applyPow rest
applyPow ((TokenTerm (Term(coef, pow))):TokenPow:(TokenNumber n):rest) = TokenTerm (Term (coef * n, pow * round n)):applyPow rest
applyPow (other:rest) = other:applyPow rest

