module SolverTests (solverTests) where

import Test.HUnit 
import Types
import Solver

solverTests :: Test
solverTests = TestList
    [ "solve simple sum" ~: solve [TokenNumber 1.0, TokenPlus, TokenNumber 2.0, 
      TokenEq, TokenVar] @?= Right (FiniteSolutions [3.0])
    ,  "solve simple sum no equals" ~: solve [TokenNumber 1.0, TokenPlus, TokenNumber 2.0] 
      @?= Right (FiniteSolutions [3.0])
    ,  "solve linear" ~: solve [TokenNumber 3.0, TokenMul, TokenVar, TokenEq, TokenNumber 6.0] 
      @?= Right (FiniteSolutions [2.0])
    ,  "solve quadratic" ~: solve [TokenVar, TokenPow, TokenNumber 2.0, TokenPlus, TokenNumber 5.0, 
      TokenMul, TokenVar, TokenPlus, TokenNumber 6.0, TokenEq, TokenNumber 0.0] @?= Right (FiniteSolutions [-3.0, -2.0])
    ,  "solve expressions" ~: solve [TokenNumber 6.0, TokenMinus, TokenExpression [TokenExpression [
      TokenNumber 2.0, TokenPlus, TokenNumber 1.0]], TokenMul, TokenNumber 2.0] @?= Right (FiniteSolutions [0.0])
    ,  "solve expansion" ~: solve [TokenExpression [TokenVar, TokenMinus, TokenNumber 2.0], TokenPow, 
      TokenNumber 2.0, TokenEq, TokenNumber 4.0] @?= Right (FiniteSolutions [0.0, 4.0])
    ,  "solve infinite solution" ~: solve [TokenVar, TokenPow, TokenNumber 9.0, TokenEq,
      TokenVar, TokenPow, TokenNumber 9.0] @?= Right InfiniteSolutions
    ,  "solve empty" ~: solve [] @?= Left "Empty input"
    ,  "solve division by zero" ~: solve [TokenNumber 1.0, TokenDiv, TokenNumber 0.0] @?= 
      Left "Division by zero"
    ,  "solve no solution" ~: solve [TokenVar, TokenPow, TokenNumber 2.0, TokenEq, 
      TokenMinus, TokenNumber 2.0] @?= Right NoRealSolutions
    ,  "solve invalid" ~: solve [TokenVar, TokenEq, TokenEq, TokenNumber 2.0] 
      @?= Left "Not a recognised expression (has TokenEq)"
    ]