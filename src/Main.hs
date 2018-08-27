module Main where

import Functions

main :: IO ()
main =
          let ex  = Unary Minus (Const 1) :: Expr Int in
          let ex1  = Binary Mult (Const 1.5) (Unary Minus (Const 0.5) ) :: Expr Double in
          let ex3  = Binary Divide (Const "a") X :: Expr String in
          let ex4  = Binary Divide (Const 2) X :: Expr Double in
           do
             putStrLn (show ex)
             putStrLn (show (evalReal ex1 22))
             putStrLn (show (diff ex3 "1"))
             putStrLn (show (diff ex4 1))
             putStrLn (show (evalReal (diff ex4 1) 10))
             putStrLn ("done")
