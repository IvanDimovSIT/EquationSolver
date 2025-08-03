module IntegrationTests (integrationTests) where

import Test.HUnit
import Tokens 
import Solver

integrationTests :: Test
integrationTests = TestList
  [ "solve simple sum" ~: solveEq "1+2" @?= Right [3.0]
  , "solve complex expression" ~: solveEq "6*12/2+10-1" @?= Right [45.0]
  , "solve square" ~: solveEq "x ^ 2 = 36" @?= Right [6.0, -6.0]
  , "solve linear" ~: solveEq "x * (5 - 3) = -8" @?= Right [-4.0]
  , "solve cube" ~: solveEq "x^3=8" @?= Right [2.0]
  , "solve quadratic 1" ~: solveEq "(2*x-4)^2=1" @?= Right [1.5, 2.5]
  , "solve quadratic 2" ~: solveEq "(x-2)^2=(-2)^2" @?= Right [0.0, 4.0]
  , "solve quadratic 3" ~: solveEq "3*(3*x*2-8+12-4+2^2)^2=3" 
    @?= Right [-0.83333333333333333333,-0.5]
  , "solve quadratic no solutions" ~: solveEq "(10-4*x)^2=(3-5)^3" @?= Right []
  , "solve invalid" ~: solveEq "(-)^2=x" 
    @?= Left "Not a recognised expression (has term TokenMinus)"
  ]


solveEq :: String -> Either String [Double]
solveEq str = do
    tokens <- parseTokens str
    solve tokens
