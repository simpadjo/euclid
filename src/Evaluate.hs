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
  IsInside p c -> undefined --TODO
  AreOnTheSameSide p1 p2 l -> undefined --TODO
  Extract e -> evaluate e >>= (\m -> case m of
                                        Just r -> Right r
                                        Nothing -> Left "Unsafe extraction failed")
  FlatMap desc ex fn -> evaluate ex >>= (\r -> evaluate (fn r))




{-describe0 :: Show p => Int -> Expr p a -> (String, Int)
describe0 currentStep expr =
  let line0 step desc = (show step) ++ ") " ++ desc in
  let line1 step desc ref = (show step) ++ ") " ++ desc ++ " from step " ++ (show ref) ++ "\n" in
  let line2 step desc ref1 ref2 = (show step) ++ ") " ++ desc ++ " from steps " ++ (show ref1) ++ " and " ++ (show ref2) ++ "\n"  in
  let describeTwo step ex1 ex2 stepName = let (part1, pos1) = describe0 step ex1 in
                                          let (part2, pos2) = describe0 pos1 ex2 in
                                          let nextLine = line2 pos2 stepName (pos1 -1) (pos2 -1) in
                                          (part1 ++ part2 ++ nextLine, pos2+1) in
  case expr of
     Point pt -> (line0 currentStep "Given a point " ++ (show pt), currentStep + 1)
     Line (a, b) -> (line0 currentStep "Draw a line between " ++ (show a) ++ " and "++ (show b) ++ "\n", currentStep + 1)
     Circle (o, (a, b)) -> (line0 currentStep "Draw a circle with the center " ++ (show o) ++ " and radius "++ (show a) ++ " -> " ++ (show b) ++ "\n", currentStep + 1)
     LLIntersect l1 l2 -> describeTwo currentStep l1 l2 "Intersect lines"
     CCIntersect c1 c2 -> describeTwo currentStep c1 c2 "Intersect circles"
     CLIntersect c l -> describeTwo currentStep c l "Intersect circle and line"
     IsInside pt c ->  describeTwo currentStep pt c "Determine if the point inside the circle"
     AreOnTheSameSide p1 p2 l -> let (part1, pos1) = describe0 currentStep p1 in
                                 let (part2, pos2) = describe0 pos1 p2 in
                                 let (part3, pos3) = describe0 pos2 l in
                                 let nextLine = (show pos3) ++ ") " ++ "Determine if two points are on the same side against the line" ++ " from steps " ++ (show pos1) ++ " "++ (show pos2)++" and " ++ (show pos3) ++ "\n"  in
                                 (part1 ++ part2 ++ part3 ++ nextLine, pos3+1)
     Extract e -> describe0 currentStep e
     FlatMap desc ex fn -> let (lines, pos) = describe0 currentStep ex in
                           let nextLine = line0 pos desc in
                           (lines ++ nextLine, pos +1)-}


main :: IO ()
main =
          let m = middlePoint (1.0, 2.0) (2.0, 3.0) in
           do
             putStrLn(show (evaluate m))
             putStrLn ("done")
