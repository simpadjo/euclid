module Main where

import Describe
import Algorithms
import Evaluate

main :: IO ()
main =
           let expr = centerOfTriangle "A" "B" "C" in
           do
             putStrLn ("How to find the center of a triangle A B C")
             putStrLn(describe expr)
             putStrLn("==============")
             putStrLn("Ok, some numerical stuff: ")
             putStrLn ("Center of a triangle (0,0) (0, 2) (2, 0)")
             putStrLn (show (evaluate (centerOfTriangle (0.0,0.0) (0.0, 2.0) (2.0, 0.0))))
