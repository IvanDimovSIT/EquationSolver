module Solver where

import Types
import qualified Data.IntMap.Strict as M
import Data.List (sortBy)

solve :: [Token] -> Either String [Double]
solve [] = Left "Empty input"
solve tokens = do
    (left, right) <- splitEquation $ convertToEquation tokens
    leftNorm <- normalise left
    rightNorm <- normalise right
    let terms = sumTerms $ moveTermsToLeft leftNorm rightNorm
    solveTerms terms

convertToEquation :: [Token] -> [Token]
convertToEquation tokens
    | hasEquals || hasVar = tokens
    | otherwise = TokenVar:TokenEq:tokens
    where
        hasEquals = TokenEq `elem` tokens
        hasVar = TokenVar `elem` tokens

splitEquation :: [Token] -> Either String ([Token], [Token])
splitEquation tokens
    | null left || null right = Left "Invalid equation"
    | otherwise = Right splitEq
    where
        splitEq@(left, right) = (takeWhile isNotEqual tokens, tailOrEmpty $ dropWhile isNotEqual tokens)
        isNotEqual token = case token of
            TokenEq -> False
            _ -> True


tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_:other) = other


normalise :: [Token] -> Either String [Term]
normalise = extractTerms
    .applyMinus
    .removePlus
    .applyMulAndDiv
    .applyPow
    .applyParenthesis
    .(Right . toTerms)


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
        _ -> Left "Unrecognised operation in parenthesis"
    | otherwise = Left "Structure not supported"
applyParenthesis (Right (TokenOpen:TokenMinus:(TokenTerm (Term (coef, pow))):TokenClose:rest)) = do
        result <- applyParenthesis $ Right rest
        Right $ TokenTerm (Term (-coef, pow)):result
applyParenthesis (Right (t:rest)) = do
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
    | coef2 == 0 = Left "Division by zero"
    | otherwise = do
        let divisionResult = TokenTerm (Term (coef1 / coef2, pow1 - pow2))
        applyMulAndDiv $ Right $ divisionResult:rest
applyMulAndDiv (Right (t:rest)) = do
    result <- applyMulAndDiv $ Right rest
    Right $ t:result


removePlus :: Either String [Token] -> Either String [Token]
removePlus = fmap $ filter (/= TokenPlus)

applyMinus :: Either String [Token] -> Either String [Token]
applyMinus (Right []) = Right []
applyMinus (Left err) = Left err
applyMinus (Right (TokenMinus:TokenTerm (Term (coef, pow)):rest)) = do
    result <- applyMinus $ Right rest
    Right $ TokenTerm (Term (-coef, pow)):result
applyMinus (Right (t:rest)) = do
    result <- applyMinus $ Right rest
    Right $ t:result


extractTerms :: Either String [Token] -> Either String [Term]
extractTerms eitherTokens = do
    tokens <- eitherTokens
    traverse traverseFn $ removeParens tokens
    where
        traverseFn t = case t of
            TokenTerm term -> Right term
            t -> Left $ "Not a recognised expression (has term " ++ show t ++ ")"

removeParens :: [Token] -> [Token]
removeParens = filter (`notElem` [TokenOpen, TokenClose])

moveTermsToLeft :: [Term] -> [Term] -> [Term]
moveTermsToLeft left right = left ++ map negateTerm right
    where
        negateTerm (Term (coef, pow)) = Term (-coef, pow)


sumTerms :: [Term] -> [Term]
sumTerms terms = convertTermMap $ foldr sumFn (M.fromList []) terms
    where
        sumFn (Term (coef, pow)) = M.alter (addToMap coef) pow
        addToMap coef found = case found of
            Just foundCoef -> Just $ foundCoef + coef
            Nothing -> Just coef


convertTermMap :: M.IntMap Double -> [Term]
convertTermMap termsMap = toTerms $ sortTerms $ M.toList termsMap
    where
        toTerms = map (\(pow, coef) -> Term (coef, pow))
        sortTerms = sortBy (\(pow1, _) (pow2, _) -> compare pow2 pow1)

solveTerms :: [Term] -> Either String [Double]
solveTerms [] = Left "Nothing to solve"
solveTerms [Term(xCoef, 1), Term(coef, 0)] = Right [-coef/xCoef]
solveTerms [Term(a, 2), Term(b, 1), Term(c, 0)] = Right $ solveQuadratic a b c
solveTerms _ = Left "Unssupported equation structure"

solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c
        | d < 0 || a == 0 = []
        | d == 0 = [x1]
        | otherwise = [x1, x2]
    where
        d = b * b - 4 * a * c
        sqrtD = sqrt d
        x1 = (-b-sqrtD)/(2*a)
        x2 = (-b+sqrtD)/(2*a)
