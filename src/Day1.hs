module Day1
    ( day1
    ) where

computeFuelForMass mass = max 0 $ (div mass 3) - 2

computeFuelForModule fuelNeeded 0 = fuelNeeded
computeFuelForModule fuelNeeded mass = 
  let
    fuel = computeFuelForMass mass
  in
    computeFuelForModule (fuelNeeded+fuel) fuel

day1 :: [Int] -> Int
day1 massList =
  sum $ map (computeFuelForModule 0) massList