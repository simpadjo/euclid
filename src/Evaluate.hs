{-#LANGUAGE GADTs, EmptyDataDecls #-}

module Evaluate where

import Geometry
import GeometryUtil
import Data.Maybe
import Control.Applicative

type NumericExpr = Expr Coords


evaluate :: NumericExpr a ->  Either String a
evaluate expr = case expr of
  Point pt -> Right pt
  Line l -> Right l
  Circle c -> Right c
  LLIntersect l1 l2 ->  liftA2 lineIntersection (evaluate l1) (evaluate l2)
  CCIntersect c1 c2 -> liftA2 circleCircleIntersection (evaluate c1) (evaluate c2)
  CLIntersect c l -> liftA2 lineCircleIntersection (evaluate l) (evaluate c)
  IsInside p c -> liftA2 isPointInCircle (evaluate p) (evaluate c)
  AreOnTheSameSide p1 p2 l -> let sameSide x y z = pointToLineOrientation x z == pointToLineOrientation y z in
                              liftA3 sameSide (evaluate p1) (evaluate p2) (evaluate l)
  Extract e -> evaluate e >>= (\m -> case m of
                                        Just r -> Right r
                                        Nothing -> Left "Unsafe extraction failed")
  FlatMap desc ex fn -> evaluate ex >>= (\r -> evaluate (fn r))
