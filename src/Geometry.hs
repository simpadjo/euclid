{-# LANGUAGE RankNTypes #-}


module Geometry where
import Data.Either
import GeometryUtil
import Control.Monad
type Radius d = (Point d, Point d)

data Line d = Line (Point d) (Point d)
  deriving (Show, Eq)

data Circle d = Circle (Point d) (Radius d)
  deriving (Show, Eq)

data Point d = Given d
             | LLIntesect (Line d) (Line d)
             | CCIntersect_1 (Circle d) (Circle d)
             | CCIntersect_2 (Circle d) (Circle d)
             | CLIntersect_1 (Circle d) (Line d)
             | CLIntersect_2 (Circle d) (Line d)
             deriving (Show, Eq)

data Shape d = P (Point d)
             | L (Line d)
             | C (Circle d)
             deriving (Show, Eq)



type TwoPointsToOneAlg = forall d. Point d -> Point d -> Point d
middlePoint :: TwoPointsToOneAlg
middlePoint p1 p2 =
  let l1 = Line p1 p2 in
  let radius = (p1, p2) in
  let circle1 = Circle p1 radius in
  let circle2 = Circle p2 radius in
  let ortogLine = Line (CCIntersect_1 circle1 circle2) (CCIntersect_2 circle1 circle2) in
  LLIntesect l1 ortogLine

--TODO: duplication
evaluate :: Point Coords -> Either String Coords
evaluate p  = case p of
  Given d -> Right d
  LLIntesect (Line p1 p2) (Line p3 p4) ->  do
                                              c1 <- evaluate p1
                                              c2 <- evaluate p2
                                              c3 <- evaluate p3
                                              c4 <- evaluate p4
                                              r <- lineIntersection (c1, c2) (c3, c4)
                                              return r
  CCIntersect_1 (Circle o1 (r11, r12)) (Circle o2 (r21, r22)) -> do
                                                    center1 <- evaluate o1
                                                    center2 <- evaluate o2
                                                    evR11 <- evaluate r11
                                                    evR12 <- evaluate r12
                                                    evR21 <- evaluate r21
                                                    evR22 <- evaluate r22
                                                    (p1, p2) <- circleCircleIntersection (center1, (evR11, evR12)) (center2, (evR21, evR22))
                                                    return p1
  CCIntersect_2 (Circle o1 (r11, r12)) (Circle o2 (r21, r22)) -> do
                                                      center1 <- evaluate o1
                                                      center2 <- evaluate o2
                                                      evR11 <- evaluate r11
                                                      evR12 <- evaluate r12
                                                      evR21 <- evaluate r21
                                                      evR22 <- evaluate r22
                                                      (p1, p2) <- circleCircleIntersection (center1, (evR11, evR12)) (center2, (evR21, evR22))
                                                      return p2
  CLIntersect_1 (Circle o1 (r1, r2)) (Line p1 p2) -> do
                                                           center <- evaluate o1
                                                           evR1 <- evaluate r1
                                                           evR2 <- evaluate r2
                                                           c1 <- evaluate p1
                                                           c2 <- evaluate p2
                                                           (p1, p2) <- lineCircleIntersection (c1, c2) (center, (evR1, evR2))
                                                           return p1
  CLIntersect_2 (Circle o1 (r1, r2)) (Line p1 p2) -> do
                                                             center <- evaluate o1
                                                             evR1 <- evaluate r1
                                                             evR2 <- evaluate r2
                                                             c1 <- evaluate p1
                                                             c2 <- evaluate p2
                                                             (p1, p2) <- lineCircleIntersection (c1, c2) (center, (evR1, evR2))
                                                             return p2


bisectAngle :: forall d. Point d -> Point d -> Point d -> Point d
bisectAngle a b c =
  let ab = Line a b in
  let circleToC = Circle b (b, c) in
  let point1 = CLIntersect_1 circleToC ab in
  let point2 = CLIntersect_1 circleToC (Line b c) in
  middlePoint point1 point2



movePointByVector :: forall d. Point d -> Point d -> Point d -> Point d
movePointByVector p v1 v2 =
  let a = CLIntersect_1 (Circle v1 (v1, v2)) (Line p v1) in
  let circle = Circle p (v1, v2) in
  let b = CLIntersect_1 circle (Line p v1) in
  CCIntersect_1 circle (Circle b (a, v2))