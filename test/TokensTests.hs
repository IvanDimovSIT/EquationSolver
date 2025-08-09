module TokensTests (tokensTests) where

import Test.HUnit
import Tokens 
import Types

tokensTests :: Test
tokensTests = TestList
    [ "parseTokens simple sum" ~: parseTokens "1+2=x" @?=
        Right [TokenNumber 1.0, TokenPlus, TokenNumber 2.0, TokenEq, TokenVar]
    , "parseTokens whitespace" ~: parseTokens " 1   + 2   = x " @?= 
        Right [TokenNumber 1.0, TokenPlus, TokenNumber 2.0, TokenEq, TokenVar]
    , "parseTokens complex" ~: parseTokens " 1+ 2*4/2^2 = x-0.5 " @?= 
        Right [TokenNumber 1.0, TokenPlus, TokenNumber 2.0, TokenMul, TokenNumber 4.0, TokenDiv, TokenNumber 2.0,
        TokenPow, TokenNumber 2.0, TokenEq, TokenVar, TokenMinus, TokenNumber 0.5] 
    , "parseTokens subexpression" ~: parseTokens "1*(2-3)=x" @?=
        Right [TokenNumber 1.0, TokenMul, TokenExpression [TokenNumber 2.0, TokenMinus, TokenNumber 3.0], 
        TokenEq, TokenVar]
    , "parseTokens multi subexpression" ~: parseTokens "(2-(5+2))=x" @?=
        Right [TokenExpression [TokenNumber 2.0, TokenMinus, TokenExpression [TokenNumber 5.0, TokenPlus, TokenNumber 2.0]], 
        TokenEq, TokenVar]
    , "parseTokens multiple subexpressions" ~: parseTokens "(-2)*(-3)=x^0.1" @?=
        Right [TokenExpression [TokenMinus, TokenNumber 2.0], TokenMul, TokenExpression [TokenMinus, TokenNumber 3.0],
        TokenEq, TokenVar, TokenPow, TokenNumber 0.1]
    ,"parseTokens implicit multiplication" ~: parseTokens "1x(5)=x" @?=
        Right [TokenNumber 1.0, TokenMul, TokenVar, TokenMul, TokenExpression [TokenNumber 5.0], TokenEq, TokenVar]
    ,"parseTokens implicit multiplication subexpression" ~: parseTokens "(5+2(10-2))2" @?=
        Right [TokenExpression [TokenNumber 5.0, TokenPlus, TokenNumber 2.0, TokenMul, 
        TokenExpression [TokenNumber 10.0, TokenMinus, TokenNumber 2.0]], TokenMul, TokenNumber 2.0]
    , "parseTokens invalid symbol" ~: parseTokens "1+@=x" @?= Left "Unrecognised character '@'"
    , "parseTokens invalid parenthesis" ~: parseTokens "1+(=x" @?= Left "Unmatched closing parenthesis"
    , "parseTokens unclosed parenthesis" ~: parseTokens "1+2=x-(2-1" @?= Left "Unmatched closing parenthesis"
    , "parseTokens nested parenthesis" ~: parseTokens "((1+2)-1=x)" @?= Left "Equal sign inside expression is not allowed"
    , "parseTokens equals in parenthesis" ~: parseTokens "(=)" @?= Left "Equal sign inside expression is not allowed"
    ]