{-#LANGUAGE GADTs, EmptyDataDecls #-}

module Evaluate where

import Geometry2
import GeometryUtil2
import Data.Maybe

type NumericExpr = Expr Coords


evaluate :: NumericExpr a ->  Either String a
evaluate expr = case expr of
  Point pt -> Right pt
  Line l -> Right l
  Circle c -> Right c
  LLIntersect l1 l2 -> do
                         e1 <- evaluate l1
                         e2 <- evaluate l2
                         return (lineIntersection e1 e2)
  CCIntersect c1 c2 -> do
                           e1 <- evaluate c1
                           e2 <- evaluate c2
                           return (circleCircleIntersection e1 e2)
  CLIntersect c l -> do
                             e1 <- evaluate c
                             e2 <- evaluate l
                             return (lineCircleIntersection e2 e1)
  IsInside p c -> do
                    p1 <- evaluate p
                    c1 <- evaluate c
                    return (isPointInCircle p1 c1)
  AreOnTheSameSide p1 p2 l -> do
                                 e1 <- evaluate p1
                                 e2 <- evaluate p2
                                 ll <- evaluate l
                                 return (pointToLineOrientation e1 ll == pointToLineOrientation e2 ll)
  Extract e -> evaluate e >>= (\m -> case m of
                                        Just r -> Right r
                                        Nothing -> Left "Unsafe extraction failed")
  FlatMap desc ex fn -> evaluate ex >>= (\r -> evaluate (fn r))


main :: IO ()
main =
          let m = middlePoint (1.0, 2.0) (2.0, 3.0) in
           do
             putStrLn(show (evaluate m))
             putStrLn ("done")
