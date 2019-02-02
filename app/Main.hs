module Main where

import Describe
import Algorithms
import Evaluate

main :: IO ()
main =
           let expr = centerOfTriangle "A" "B" "C" in
           do
             print "How to find the center of a triangle A B C"
             putStrLn $ describe expr
             print "=============="
             print "Ok, some numerical stuff: "
             print "Center of a triangle (0,0) (0, 2) (2, 0)"
             print $ show $ evaluate $ centerOfTriangle (0.0, 0.0) (0.0, 2.0) (2.0, 0.0)
