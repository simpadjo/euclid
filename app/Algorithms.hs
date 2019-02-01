{-# LANGUAGE RankNTypes #-}

module Algorithms where

import Geometry

type TwoPointsToOneAlg = forall p. p -> p -> Expr p p
type ThreePointsToOneAlg = forall p. p -> p -> p -> Expr p p

middlePoint :: TwoPointsToOneAlg
middlePoint p1 p2 =
  let l1 = Line (p1, p2) in
  let radius = (p1, p2) in
  let circle1 = Circle (p1, radius) in
  let circle2 = Circle (p2, radius) in
  let twoPoints = Extract (CCIntersect circle1 circle2) in
  let orthogLine = FlatMap "line between two points" twoPoints Line in
  Extract (LLIntersect l1 orthogLine)


bisectAngle :: ThreePointsToOneAlg
bisectAngle a b c =
  let ab = Line (a, b) in
  let circleToC = Circle (b, (b, c)) in
  let twoPoints = Extract (CLIntersect circleToC ab) in
  FlatMap "middle point" twoPoints (uncurry middlePoint)


centerOfTriangle :: ThreePointsToOneAlg
centerOfTriangle a b c =
  let a1 = middlePoint b c in
  let b1 = middlePoint a c in
  let aa1 = FlatMap "line a a1" a1 (\x -> Line (a, x)) in
  let bb1 = FlatMap "line b b1" b1 (\x -> Line (b, x)) in
  Extract (LLIntersect aa1 bb1)
