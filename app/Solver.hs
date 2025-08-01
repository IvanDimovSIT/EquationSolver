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
normalise terms = do
    normalisedExpressions <- normaliseExpressions terms
    let normalisedToTerms = toTerms normalisedExpressions
    termsWithPowersApplied <- applyPow normalisedToTerms
    termsWithMulDivApplied <- applyMulAndDiv termsWithPowersApplied
    termsWithMinusApplied <- applyMinus termsWithMulDivApplied
    let termsWithPlusApplied = removePlus termsWithMinusApplied
    extractTerms termsWithPlusApplied

normaliseExpressions :: [Token] -> Either String [Token]
normaliseExpressions [] = Right []
normaliseExpressions ((TokenExpression expr):rest) = do
    normalizedExpr <- normalise expr
    restNormalized <- normaliseExpressions rest
    Right $ TokenTermExpression (sumTerms normalizedExpr):restNormalized
normaliseExpressions (t:rest) = do
    restNormalized <- normaliseExpressions rest
    Right $ t:restNormalized

toTerms :: [Token] -> [Token]
toTerms [] = []
toTerms ((TokenNumber n):rest) = TokenTerm (Term (n, 0)):toTerms rest
toTerms (TokenVar:rest) = TokenTerm (Term (1, 1)):toTerms rest
toTerms (t:rest) = t:toTerms rest

applyPow :: [Token] -> Either String [Token]
applyPow [] = Right []
applyPow ((TokenTerm (Term(coef1, pow1))):TokenPow:(TokenTerm (Term(coef2, pow2))):rest)
    | pow2 == 0 = do
        result <- applyPow rest
        Right $ TokenTerm (Term (coef1**coef2,pow1*round coef2)):result
    | otherwise = Left "Power of X not supported"
applyPow (TokenPow:TokenTermExpression _:_) = Left "Powers of expressions not supported"
applyPow ((TokenTermExpression expr):TokenPow:(TokenTerm term):rest) = do
    exprResult <- applyPowToExpression expr term
    othersResult <- applyPow rest
    Right $ TokenTermExpression exprResult:othersResult
applyPow (other:rest) = do
    result <- applyPow rest
    Right $ other:result

applyMulAndDiv :: [Token] -> Either String [Token]
applyMulAndDiv [] = Right []
applyMulAndDiv ((TokenTerm (Term(coef1,pow1))):TokenMul:(TokenTerm (Term(coef2,pow2))):rest) = do
    let product = TokenTerm (Term (coef1 * coef2, pow1 + pow2))
    applyMulAndDiv $ product:rest
applyMulAndDiv ((TokenTerm (Term(coef1,pow1))):TokenDiv:(TokenTerm (Term(coef2,pow2))):rest)
    | coef2 == 0 = Left "Division by zero"
    | otherwise = do
        let divisionResult = TokenTerm (Term (coef1 / coef2, pow1 - pow2))
        applyMulAndDiv $ divisionResult:rest
applyMulAndDiv (TokenTerm term:TokenMul:TokenTermExpression expr:rest) = do
    let exprResult = applyMulToTerms expr term
    applyMulAndDiv $ TokenTermExpression exprResult:rest
applyMulAndDiv ((TokenTermExpression expr):TokenMul:TokenTerm term:rest) = do
    let exprResult = applyMulToTerms expr term
    applyMulAndDiv $ TokenTermExpression exprResult:rest
applyMulAndDiv ((TokenTermExpression expr):TokenDiv:TokenTerm term:rest) = do
    exprResult <- applyDivToTerms expr term
    applyMulAndDiv $ TokenTermExpression exprResult:rest
applyMulAndDiv (t:rest) = do
    result <- applyMulAndDiv rest
    Right $ t:result

applyMulToTerms :: [Term] -> Term -> [Term]
applyMulToTerms terms (Term (coef, pow)) = map mapFn terms
    where
        mapFn (Term (c, p)) = Term (c * coef, p + pow)

applyDivToTerms :: [Term] -> Term -> Either String [Term]
applyDivToTerms terms (Term (coef, pow))
    | coef == 0 = Left "Division by zero"
    | otherwise = Right $ map mapFn terms
    where
        mapFn (Term (c, p)) = Term (c / coef, p - pow)

applyPowToExpression :: [Term] -> Term -> Either String [Term]
applyPowToExpression _ (Term(0, _)) = Right [Term (0, 0)]
applyPowToExpression terms (Term(1, 0)) = Right terms
applyPowToExpression [Term (coef, pow)] (Term(coef2, 0)) = Right [Term (coef**coef2, pow * round coef2)]
applyPowToExpression [Term(c1, p1), Term(c2, p2)] (Term(coef, pow))
    | pow /= 0 = Left "Power of X not supported in expressions"
    | coef == 2.0 = Right [Term (c1^pow, p1*pow), Term (2 * c1 * c2, p1 + p2), Term (c2^pow, p2*pow)]
    | otherwise = Left $ "Unsupported expression for power of " ++ show coef
applyPowToExpression terms pow = Left $ "Unsupported expression for power operation:" ++ show terms ++ "^" ++ show pow

removePlus :: [Token] -> [Token]
removePlus = filter (/= TokenPlus)

applyMinus :: [Token] -> Either String [Token]
applyMinus [] = Right []
applyMinus (TokenMinus:TokenTerm (Term (coef, pow)):rest) = do
    result <- applyMinus rest
    Right $ TokenTerm (Term (-coef, pow)):result
applyMinus (TokenMinus:TokenTermExpression terms:rest) = do
    result <- applyMinus rest
    Right $ TokenTermExpression (applyMinusToTerms terms):result
applyMinus (t:rest) = do
    result <- applyMinus rest
    Right $ t:result

applyMinusToTerms :: [Term] -> [Term]
applyMinusToTerms = map (\(Term (coef, pow)) -> Term (-coef, pow))

extractTerms :: [Token] -> Either String [Term]
extractTerms tokens = do
    termGroups <- traverse traverseFn $ removeParens tokens
    Right $ concat termGroups
    where
        traverseFn t = case t of
            TokenTerm term -> Right [term]
            TokenTermExpression terms -> Right terms
            _ -> Left $ "Not a recognised expression (has term " ++ show t ++ ")"

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
solveTerms [_] = Right [0.0]
solveTerms [Term(xCoef, 1), Term(coef, 0)] = Right [-coef/xCoef]
solveTerms [Term(coef, pow), Term(value, 0)] = solvePowerEquation coef pow value
solveTerms [Term(a, 2), Term(b, 1), Term(c, 0)] = Right $ solveQuadratic a b c
solveTerms terms = Left $ "Unsupported equation structure:" ++ invalidTerms
    where
        invalidTerms = unwords (map show terms)

-- a*x^b + c = 0
solvePowerEquation :: Double -> Int -> Double -> Either String [Double]
solvePowerEquation a b c
    | a == 0 && c == 0 = Left "Infinitely many solutions"
    | a == 0 = Left "No solutions"
    | c == 0 = Right [0]
    | (-c)/a < 0 && even b = Left "No real solutions"
    | even b = Right [firstSolution, secondSolution]
    | otherwise = Right [firstSolution]
    where
        firstSolution = ((-c)/a)**(1.0/fromIntegral b)
        secondSolution = -firstSolution

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
