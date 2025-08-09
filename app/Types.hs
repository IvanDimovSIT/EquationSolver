module Types where

data Token = TokenPlus 
    | TokenMinus 
    | TokenMul 
    | TokenDiv 
    | TokenPow 
    | TokenOpen 
    | TokenClose
    | TokenEq
    | TokenVar
    | TokenTerm Term
    | TokenNumber Double
    | TokenExpression [Token]
    -- | Normalised expression
    | TokenTermExpression [Term]
    deriving(Show, Eq)

-- | (Coef, Pow)
type Term = (Double, Int)
