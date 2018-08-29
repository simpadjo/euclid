module GeomMain where

import Geometry
import GeometryUtil

main :: IO ()
main =
         {- let ex  = Unary Minus (Const 1) :: Expr Int in
          let ex1  = Binary Mult (Const 1.5) (Unary Minus (Const 0.5) ) :: Expr Double in
          let ex3  = Binary Divide (Const "a") X :: Expr String in
          let ex4  = Binary Divide (Const 2) X :: Expr Double in-}
          let m = middlePoint (Given "A") (Given "B") :: (Point String) in
          let mm = middlePoint (Given (1.0, 2.0)) (Given (5.0, 4.0)) :: (Point Coords) in
          --let bis = bisectAngle (Given (1.0, 1.0)) (Given (0.0, 0.0)) (Given (1.0, -1.0)) in
          let bis = bisectAngle (Given (0.0, 1.0)) (Given (0.0, 0.0)) (Given (3.0, 0.0)) in
           do
             putStrLn(show m)
             putStrLn(show (evaluate mm))
             putStrLn(show (evaluate bis))
             putStrLn ("done")
