module GeometryUtil where

eps = 0.001 :: Double
type Coords = (Double, Double)
lineIntersection :: (Coords, Coords) -> (Coords, Coords) -> Either String Coords
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4) in
  if (abs denom) < eps
    then Left "No intersection of parallel lines"
    else let a1 = (x1*y2 - y1*x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3*x4) in
         let a2 = (x1*y2 - y1*x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3*x4) in
         Right (a1 / denom, a2 / denom)

distSqare :: (Coords, Coords) -> Double
distSqare ((x1, y1), (x2, y2)) = (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)


--https://math.stackexchange.com/questions/256100/how-can-i-find-the-points-at-which-two-circles-intersect#256123
circleCircleIntersection :: (Coords, (Coords, Coords)) -> (Coords, (Coords, Coords)) -> Either String (Coords, Coords)
circleCircleIntersection ((x1, y1), rad1) ((x2, y2), rad2) =
  let r1sq = distSqare rad1 in
  let r2sq = distSqare rad2 in
  let distSq = distSqare ((x1, y1), (x2, y2)) in
  let mult = (r1sq - r2sq) / (2*distSq) in
  let underTheRoot = 2* (r1sq + r2sq) / distSq - (r1sq - r2sq) * (r1sq - r2sq)/(distSq * distSq) -1 in
  if underTheRoot < eps
    then Left "circles don't intersect"
    else
      let mult2 = (sqrt underTheRoot) / 2 in
      Right ( ( (x1 + x2)/2 + mult*(x2 -x1) - mult2*(y2 - y1),(y1 + y2)/2 + mult*(y2 -y1) - mult2*(x1 - x2)),
              ((x1 + x2)/2 + mult*(x2 -x1) + mult2*(y2 - y1),(y1 + y2)/2 + mult*(y2 -y1) + mult2*(x1 - x2)) )


--http://www.ambrsoft.com/TrigoCalc/Circles2/circlrLine_.htm
lineCircleIntersection :: (Coords, Coords) -> (Coords, (Coords, Coords)) -> Either String (Coords, Coords)
lineCircleIntersection ((x1, y1), (x2, y2)) ((a, b), rad) =
   if abs (x1 - x2) < eps
   then lineCircleIntersectionVertical ((x1, y1), (x2, y2)) ((a, b), rad)
   else lineCircleIntersectionRegular ((x1, y1), (x2, y2)) ((a, b), rad)

lineCircleIntersectionVertical :: (Coords, Coords) -> (Coords, (Coords, Coords)) -> Either String (Coords, Coords)
lineCircleIntersectionVertical ((x1, y1), (x2, y2)) ((a, b), rad) =
  let rSq = distSqare rad in
  let deltaSq = rSq - (a - x1) * (a - x1) in
  if deltaSq < eps
  then Left "Line and circle don't intersect"
  else Right ((x1, b - (sqrt deltaSq)) , (x1, b + (sqrt deltaSq)))


lineCircleIntersectionRegular :: (Coords, Coords) -> (Coords, (Coords, Coords)) -> Either String (Coords, Coords)
lineCircleIntersectionRegular ((x1, y1), (x2, y2)) ((a, b), rad) =
  let rSq = distSqare rad in
  let m = (y2 - y1) / (x2 - x1) in
  let d = y1 - m*x1 in
  let delta = rSq * (1 + m*m) - (b - m*a-d) * (b - m*a-d) in
  if delta < eps
    then Left "Line and circle don't intersect"
    else
      let resX1 = (a + b*m - d*m - (sqrt delta)) / (1 + m*m) in
      let resX2 = (a + b*m - d*m + (sqrt delta)) / (1 + m*m) in
      Right ((resX1, resX1*m + d), (resX2, resX2*m + d))
