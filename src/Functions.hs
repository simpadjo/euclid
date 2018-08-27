module Functions where

data UnOp = Minus | Inverse
  deriving (Eq, Show)

data BinOp = Add | Mult | Divide
  deriving (Eq, Show)

data Expr a = Zero
          | Const a
          | X
          | Unary UnOp (Expr a)
          | Binary BinOp (Expr a) (Expr a)
           deriving (Eq, Show)

evalReal :: Expr Double -> Double-> Double
evalReal expr x = case expr of
  Zero -> 0
  Const ex -> ex
  X -> x
  Unary Minus ex -> -1 * (evalReal ex x)
  Unary Inverse ex -> 1 / (evalReal ex x)
  Binary Add ex1 ex2 -> (evalReal ex1 x) + (evalReal ex2 x)
  Binary Mult ex1 ex2 -> (evalReal ex1 x) * (evalReal ex2 x)
  Binary Divide ex1 ex2 -> (evalReal ex1 x) / (evalReal ex2 x)


diff :: Expr a -> a -> Expr a
diff expr one = case expr of
              Zero -> Zero
              Const _ -> Zero
              X -> Const(one)
              Unary Minus ex -> Unary Minus (diff ex one)
              Unary Inverse ex -> Unary Minus (Unary Inverse (Binary Mult ex ex))
              Binary Add ex1 ex2 -> Binary Add (diff ex1  one) (diff ex2 one)
              Binary Mult ex1 ex2 -> Binary Add (Binary Mult (diff ex1 one) ex2) (Binary Mult (diff ex2 one) ex1)
              Binary Divide ex1 ex2 -> Binary Divide (Binary Add (Binary Mult (diff ex1 one) (ex2)) (Unary Minus (Binary Mult (diff ex2 one) (ex1))) ) (Binary Mult ex2 ex2 )
