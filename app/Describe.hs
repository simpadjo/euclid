{-#  LANGUAGE GADTs  #-}

module Describe where

import Geometry

describe' :: Show p => Int -> Expr p a -> (String, Int)
describe' currentStep expr =
  let line0 step desc = show step ++ ") " ++ desc in
  let line2 step desc ref1 ref2 = show step ++ ") " ++ desc ++ " from steps " ++ show ref1 ++ " and " ++ show ref2 ++ "\n"  in
  let describeTwo step ex1 ex2 stepName = let (part1, pos1) = describe' step ex1 in
                                          let (part2, pos2) = describe' pos1 ex2 in
                                          let nextLine = line2 pos2 stepName (pos1 -1) (pos2 -1) in
                                          (part1 ++ part2 ++ nextLine, pos2+1) in
  case expr of
     Point pt -> (line0 currentStep "Given a point " ++ show pt, currentStep + 1)
     Line (a, b) -> (line0 currentStep "Draw a line between " ++ show a ++ " and "++ show b ++ "\n", currentStep + 1)
     Circle (o, (a, b)) -> (line0 currentStep "Draw a circle with the center " ++ show o ++ " and radius "++ show a ++ " -> " ++ show b ++ "\n", currentStep + 1)
     LLIntersect l1 l2 -> describeTwo currentStep l1 l2 "Intersect lines"
     CCIntersect c1 c2 -> describeTwo currentStep c1 c2 "Intersect circles"
     CLIntersect c l -> describeTwo currentStep c l "Intersect circle and line"
     IsInside pt c ->  describeTwo currentStep pt c "Determine if the point inside the circle"
     AreOnTheSameSide p1 p2 l -> let (part1, pos1) = describe' currentStep p1 in
                                 let (part2, pos2) = describe' pos1 p2 in
                                 let (part3, pos3) = describe' pos2 l in
                                 let nextLine = show pos3 ++ ") " ++ "Determine if two points are on the same side against the line" ++ " from steps " ++ show pos1 ++ " "++ show pos2 ++" and " ++ show pos3 ++ "\n"  in
                                 (part1 ++ part2 ++ part3 ++ nextLine, pos3+1)
     Extract e -> describe' currentStep e
     FlatMap desc ex _ -> let (aggLines, pos) = describe' currentStep ex in
                           let nextLine = line0 pos desc in
                           (aggLines ++ nextLine ++ "\n", pos +1)


--TODO: deduplication of messages
describe :: Show p => Expr p a -> String
describe expr = fst (describe' 1 expr)
