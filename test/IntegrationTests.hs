module IntegrationTests (integrationTests) where

import Test.HUnit
import Tokens 
import Solver
import Types

integrationTests :: Test
integrationTests = TestList
    [ "solve simple sum" ~: solveEq "1+2" @?= Right (FiniteSolutions [3.0])
    , "solve complex expression" ~: solveEq "6*12/2+10-1" @?= Right (FiniteSolutions [45.0])
    , "solve sub expression" ~: solveEq "(12-2*((5-2)*2))" @?= Right (FiniteSolutions [0.0])
    , "solve implicit multiplication" ~: solveEq "6x=(-2)x+8" @?= Right (FiniteSolutions [1.0])
    , "solve implicit multiplication sub expression" ~: solveEq 
      "(5+2(10-2))2" @?= Right (FiniteSolutions [42.0])
    , "solve square" ~: solveEq "x ^ 2 = 36" @?= Right (FiniteSolutions [6.0, -6.0])
    , "solve linear" ~: solveEq "x * (5 - 3) = -8" @?= Right (FiniteSolutions [-4.0])
    , "solve cube" ~: solveEq "x^3=8" @?= Right (FiniteSolutions [2.0])
    , "solve quadratic 1" ~: solveEq "(2*x-4)^2=1" @?= Right (FiniteSolutions[1.5, 2.5])
    , "solve quadratic 2" ~: solveEq "(x-2)^2=(-2)^2" @?= Right (FiniteSolutions [0.0, 4.0])
    , "solve quadratic 3" ~: solveEq "3*(3*x*2-8+12-4+2^2)^2=3" 
      @?= Right (FiniteSolutions [-0.83333333333333333333,-0.5])
    , "solve quadratic no solutions" ~: solveEq "(10-4*x)^2=(3-5)^3" @?= Right NoRealSolutions
    , "solve infinite solutions" ~: solveEq "4x-x=2x+x" @?= Right InfiniteSolutions
    , "solve invalid" ~: solveEq "(-)^2=x" 
      @?= Left "Not a recognised expression (has TokenMinus)"
    , "solve division by zero" ~: solveEq "10/(8-2(2^2))" @?= Left "Division by zero" 
    ]

solveEq :: String -> Either String SolutionResult
solveEq str = parseTokens str >>= solve
