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
    deriving(Show)

newtype Term = Term (Double, Int)
    deriving(Show)
