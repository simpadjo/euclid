{-#LANGUAGE GADTs #-}

module Geometry where

type LineAB p = (p, p)
type CircleOAB p = (p, (p, p))

data Expr p a where
   Point :: p -> Expr p p
   Line :: LineAB p -> Expr p (LineAB p)
   Circle :: CircleOAB p -> Expr p (CircleOAB p)
   LLIntersect :: Expr p (LineAB p) -> Expr p (LineAB p) -> Expr p (Maybe p)
   CCIntersect :: Expr p (CircleOAB p) -> Expr p (CircleOAB p) -> Expr p (Maybe (p ,p))
   CLIntersect :: Expr p (CircleOAB p) -> Expr p (LineAB p) -> Expr p (Maybe (p ,p))
   IsInside :: Expr p p -> Expr p (CircleOAB p) -> Expr p Bool
   AreOnTheSameSide :: Expr p p -> Expr p p -> Expr p (LineAB p) -> Expr p Bool
   Extract :: Expr p (Maybe x) -> Expr p x
   FlatMap :: String -> Expr p x -> (x -> Expr p y) -> Expr p y
