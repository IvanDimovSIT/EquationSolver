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
    | TokenTermExpression [Term]
    deriving(Show, Eq)

newtype Term = Term (Double, Int)
    deriving(Show, Eq)
