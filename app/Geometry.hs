{-#LANGUAGE GADTs #-}

module Geometry where

type LineAB p = (p, p)
type CircleOAB p = (p, (p, p))

data Expr p a where
   Point :: p -> Expr p p
   Line :: LineAB p -> Expr p (LineAB p)
   Circle :: CircleOAB p -> Expr p (CircleOAB p)
   LLIntersect :: (Expr p (LineAB p)) -> (Expr p (LineAB p)) -> Expr p (Maybe p)
   CCIntersect :: (Expr p (CircleOAB p)) -> (Expr p (CircleOAB p)) -> Expr p (Maybe (p ,p))
   CLIntersect :: (Expr p (CircleOAB p)) -> (Expr p (LineAB p)) -> Expr p (Maybe (p ,p))
   IsInside :: (Expr p p) -> (Expr p (CircleOAB p)) -> Expr p Bool
   AreOnTheSameSide :: (Expr p p) -> (Expr p p) -> (Expr p (LineAB p)) -> Expr p Bool
   Extract :: (Expr p (Maybe x)) -> (Expr p x)
   FlatMap :: String -> (Expr p x) -> (x -> Expr p y) -> (Expr p y)


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
    FlatMap desc e _ -> "Flatmap " ++ desc ++ " to " ++(debugExpr e)

