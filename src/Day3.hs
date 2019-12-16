{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( day3,
    day3part2
  ) where

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.List.Partial ((!!))
import Text.Read as TR

data Direction = Up | Down | Left | Right

type Point = (Int, Int)

data Motion =
  Motion {
    direction :: Direction,
    distance :: Point
  }

data Segment =
  Segment {
    start :: Point,
    end :: Point
  }

day3 :: [[String]] -> Int
day3 lines =
  let
    player1 = map Day3.fromString (lines !! 0)
    player2 = fromMotion $ map Day3.fromString (lines !! 1)
  in
    

day3 :: [[String]] -> Int
day3 lines =
  let
    player1 = map Day3.fromString (lines !! 0)
    player2 = map Day3.fromString (lines !! 1)
    intersectOp = \start motion -> findSegmentCrossingLines (0, 0) player1 start motion []
  in
    traversePlayerAndFindIntersections (\(a,b) -> (abs a) + (abs b)) intersectOp (0,0) player2

day3part2 :: [[String]] -> Int
day3part2 lines =
  let
    player1 = map Day3.fromString (lines !! 0)
    player2 = map Day3.fromString (lines !! 1)
    intersectOp = \start motion -> findSegmentCrossingLines (0, 0) player1 start motion []
    stepCost = \point -> (stepsToPoint player1 point) + (stepsToPoint player2 point)
  in
    traversePlayerAndFindIntersections stepCost intersectOp (0,0) player2

traversePlayerAndFindIntersections _ _ _ [] = maxBound
traversePlayerAndFindIntersections costFx intersectOp basePoint (firstMotion:playerMotions) =
  let
    newBasePoint = movePoint basePoint firstMotion
    intersectPoints = intersectOp basePoint firstMotion
    sortedDistances = L.sort $ map costFx intersectPoints
    bestDistance = (L.headMaybe sortedDistances)
  in case bestDistance of
    Nothing -> traversePlayerAndFindIntersections costFx intersectOp newBasePoint playerMotions
    Just best -> min best $ traversePlayerAndFindIntersections costFx intersectOp newBasePoint playerMotions

findSegmentCrossingLines :: (Int, Int) -> [Motion] -> (Int, Int) -> Motion -> [(Int, Int)] -> [(Int, Int)]
findSegmentCrossingLines origin [] startPoint motion intersections = intersections
findSegmentCrossingLines origin (newFrame:frameMotions) startPoint motion intersections =
  let
    (ox, oy) = origin
    newOrigin = movePoint origin newFrame
    newStartPoint = moveFrame startPoint newFrame
    endPoint = movePoint startPoint motion
    newEndPoint = moveFrame endPoint newFrame
    vectorToIntersect = isAnIntersection startPoint endPoint newStartPoint newEndPoint
  in case vectorToIntersect of
    Nothing -> findSegmentCrossingLines newOrigin frameMotions newStartPoint motion intersections
    Just (offx, offy) ->
      findSegmentCrossingLines newOrigin frameMotions newStartPoint motion (
        intersections ++ [(ox+offx, oy+offy)]
      )

stepsToPoint :: [Motion] -> (Int, Int) -> Int
stepsToPoint motions point =
  _stepsToPoint motions (0,0) point 0

_stepsToPoint [] _ _ count = count
_stepsToPoint (motion:restOfMotions) origin point count =
  let
    newOrigin = movePoint origin motion
    stepsToTake = _stepsAlongLineToPoint motion origin point 0
    Motion _ totalStepsAlongLine = motion
  in
    if totalStepsAlongLine == stepsToTake then
      _stepsToPoint restOfMotions newOrigin point (count + stepsToTake)
    else
      count + stepsToTake

_stepsAlongLineToPoint (Motion dir 0) origin point count = count
_stepsAlongLineToPoint (Motion dir dist) origin point count =
  let
    oneStep = Motion dir 1
    newOrigin = movePoint origin oneStep
  in
    if origin == point then
      count
    else
      _stepsAlongLineToPoint (Motion dir (dist-1)) newOrigin point (count + 1)

isAnIntersection :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
isAnIntersection (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  let
    happened = if x1 == x2
      then ((signum (x1*x3)) <= 0) && ((signum (y1*y2)) < 0)
      else ((signum (x1*x2)) < 0) && ((signum (y1*y3)) <= 0)
  in case happened of
    True -> if x1 == x2 then Just (x1, 0) else Just (0, y1)
    False -> Nothing

fromString ('U':s) =
  Motion {
    direction = Day3.Up,
    distance = TR.read s
  }
fromString ('D':s) =
  Motion {
    direction = Day3.Down,
    distance = TR.read s
  }
fromString ('L':s) =
  Motion {
    direction = Day3.Left,
    distance = TR.read s
  }
fromString ('R':s) =
  Motion {
    direction = Day3.Right,
    distance = TR.read s
  }

x :: Point -> Int
x p = fst p

y :: Point -> Int
y p = snd p

fromMotion :: [Motion] -> [Segment]
fromMotion motions =
  _fromMotion (0, 0) motions []

_fromMotion :: (Int, Int) -> [Motion] -> [Segment] -> [Segment]
_fromMotion _ [] path = path
_fromMotion startPoint (motion:restOfMotions) path =
  let
    endPoint = movePoint startPoint motion
  in
    _fromMotion endPoint restOfMotions $ Segment startPoint endPoint

moveSegment :: Segment -> Motion -> Segment
moveSegment seg motion =
  Segment (movePoint (start seg) motion) (movePoint (end seg) motion)

movePoint :: (Int, Int) -> Motion -> (Int, Int)
movePoint (x,y) (Motion Day3.Up d) = (x, y+d)
movePoint (x,y) (Motion Day3.Down d) = (x, y-d)
movePoint (x,y) (Motion Day3.Right d) = (x+d, y)
movePoint (x,y) (Motion Day3.Left d) = (x-d, y)

moveFrame :: (Int, Int) -> Motion -> (Int, Int)
moveFrame (x,y) (Motion Day3.Up d) = (x, y-d)
moveFrame (x,y) (Motion Day3.Down d) = (x, y+d)
moveFrame (x,y) (Motion Day3.Right d) = (x-d, y)
moveFrame (x,y) (Motion Day3.Left d) = (x+d, y)
