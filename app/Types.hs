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

type Term = (Double, Int)
