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


type Point = (Int, Int)

data Direction = Up | Down | Left | Right

-- Captures a basic translation transform.
data Motion =
  Motion {
    direction :: Direction,
    distance :: Int
  }

-- Stores a single line segment.
data Segment =
  Segment {
    start :: Point,
    end :: Point
  }

day3 :: [[String]] -> Int
day3 lines =
  let
    player1 = map Day3.fromString (lines !! 0)
    player2 = fromMotions $ map Day3.fromString (lines !! 1)
    intersectionPoints = L.foldl (++) [] $ map (intersections player1) player2
    costFx = \(a,b) -> (abs a) + (abs b)
  in
    L'.head . L.sort . (map costFx) $ intersectionPoints

day3part2 :: [[String]] -> Int
day3part2 lines =
  let
    player1 = map Day3.fromString (lines !! 0)
    motionPlayer2 = map Day3.fromString (lines !! 1)
    player2 = fromMotions motionPlayer2
    intersectionPoints = L.foldl (++) [] $ map (intersections player1) player2
    stepCost = \point -> (stepsToPoint player1 point) + (stepsToPoint motionPlayer2 point)
  in
    L'.head . L.sort . (map stepCost) $ intersectionPoints

intersections :: [Motion] -> Segment -> [(Int, Int)]
intersections motions segment = _intersections (0, 0) motions segment []

_intersections _ [] _ result = result
_intersections origin (motion:restOfMotions) segment intersectPoints  =
  let
    (ox, oy) = origin
    newOrigin = movePoint origin motion
    newSegment = moveFrameForSegment segment motion
    intersectOffset = segmentsCrossOriginOffset segment newSegment
    recursiveStep = _intersections newOrigin restOfMotions newSegment
  in case intersectOffset of
    Nothing -> recursiveStep intersectPoints
    Just (offx, offy) ->
      recursiveStep $ intersectPoints ++ [(ox+offx, oy+offy)]

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

segmentsCrossOriginOffset :: Segment -> Segment -> Maybe (Int, Int)
segmentsCrossOriginOffset s1 s2 =
  isAnIntersection (start s1) (end s1) (start s2) (end s2)

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

fromMotions :: [Motion] -> [Segment]
fromMotions motions =
  _fromMotions (0, 0) motions []

_fromMotions :: (Int, Int) -> [Motion] -> [Segment] -> [Segment]
_fromMotions _ [] path = path
_fromMotions startPoint (motion:restOfMotions) path =
  let
    endPoint = movePoint startPoint motion
    newSegment = Segment{ start=startPoint, end=endPoint }
  in
    _fromMotions endPoint restOfMotions $ path ++ [newSegment]

moveSegment :: Segment -> Motion -> Segment
moveSegment seg motion =
  Segment (movePoint (start seg) motion) (movePoint (end seg) motion)

moveFrameForSegment :: Segment -> Motion -> Segment
moveFrameForSegment seg motion =
  Segment (moveFrame (start seg) motion) (moveFrame (end seg) motion)

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
