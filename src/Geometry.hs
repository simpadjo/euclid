{-#LANGUAGE GADTs #-}

module Geometry where
import Data.Either
import Control.Monad

data AbstractExpr p l c a where
   Point :: p -> AbstractExpr p l c p
   Line :: l -> AbstractExpr p l c l
   Circle :: c -> AbstractExpr p l c c
   LLIntersect :: (AbstractExpr p l c l) -> (AbstractExpr p l c l) -> AbstractExpr p l c (Maybe p)
   CCIntersect :: (AbstractExpr p l c c) -> (AbstractExpr p l c c) -> AbstractExpr p l c (Maybe (p ,p))
   CLIntersect :: (AbstractExpr p l c c) -> (AbstractExpr p l c l) -> AbstractExpr p l c (Maybe (p ,p))
   IsInside :: (AbstractExpr p l c p) -> (AbstractExpr p l c c) -> AbstractExpr p l c Bool
   AreOnTheSameSide :: (AbstractExpr p l c p) -> (AbstractExpr p l c p) -> (AbstractExpr p l c l) -> AbstractExpr p l c Bool
   Extract :: (AbstractExpr p l c (Maybe x)) -> (AbstractExpr p l c x)
   FlatMap :: String -> (AbstractExpr p l c x) -> (x -> AbstractExpr p l c y) -> (AbstractExpr p l c y)


type LineAB p = (p, p)
type CircleOAB p = (p, (p, p))
type Expr p = AbstractExpr p (LineAB p) (CircleOAB p)


debugExpr :: Show p =>  Expr p a -> String
debugExpr expr =
  case expr of
    Point pt -> show pt
    Line (a, b) -> "Line "++ (show a) ++ ", " ++ (show b)
    Circle (o, (a, b)) -> "Circle "++ (show o) ++ ", " ++ (show a) ++ " - " ++ (show b)
    LLIntersect l1 l2 -> "LL " ++ (debugExpr l1) ++ " " ++(debugExpr l2)
    CCIntersect c1 c2 -> "CC " ++ (debugExpr c1) ++ " " ++(debugExpr c2)
    CLIntersect c l -> "CC " ++ (debugExpr c) ++ " " ++(debugExpr l)
    IsInside pt c -> "IsInside " ++ (debugExpr pt) ++ " " ++(debugExpr c)
    AreOnTheSameSide p1 p2 l -> "AreOnTheSameSide " ++ (debugExpr p1) ++ " " ++(debugExpr p2) ++ " " ++ (debugExpr l)
    Extract e ->  "Extract " ++ (debugExpr e)
    FlatMap desc e fn -> "Flatmap " ++ desc ++ " to " ++(debugExpr e)

