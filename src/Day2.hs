module Day2
  ( day2, day2part2
  ) where

import Data.List

second (a,b) = b

replace :: Int -> Int -> [Int] -> [Int]
replace index value list =
  let
    ilist = zip [0..] list
    (ibefore, iafter) = partition (\(a,b) -> a < index) ilist
    before = map second ibefore
    after = map second iafter
  in
    before ++ [value] ++ (tail after)

makeNewState :: [Int] -> Int -> Int -> [Int]
makeNewState state a b =
  let
    replace1 = replace 1 a
    replace2 = replace 2 b
  in
    replace1 . replace2 $ state

make1201state state =
  makeNewState state 12 2

operate index 1 parameters state =
  let
    arg1 = state !! (parameters !! 0)
    arg2 = state !! (parameters !! 1)
  in
    computeStep (index+4) $
      replace (parameters !! 2) (arg1+arg2) state

operate index 2 parameters state =
  let
    arg1 = state !! (parameters !! 0)
    arg2 = state !! (parameters !! 1)
  in
    computeStep (index+4) $
      replace (parameters !! 2) (arg1*arg2) state

operate index 99 parameters state = state

operate index x parameters state = [-1] -- le bad

splitState :: Int -> [Int] -> ([Int], Int, [Int], [Int])
splitState index state =
  let
    (before, include) = splitAt index state
    (operation, rest) = splitAt 4 include
  in
    (before, head operation, tail operation, rest)

computeStep :: Int -> [Int] -> [Int]
computeStep index state =
  let
    (_, opcode, parameters, _) = splitState index state
  in
    operate index opcode parameters state


day2 :: [Int] -> Int
day2 state =
  let
    startState = make1201state state
  in
    head $ computeStep 0 startState

-- Find input that gives output
day2part2 :: [Int] -> Int
day2part2 state = day2part2_internal state 0

day2part2_internal state 10000 = -1 --failed to find

day2part2_internal state i =
  let
    startState = makeNewState state (div i 100) (mod i 100)
    result = head $ computeStep 0 startState
  in
    day2part2_check state i result

day2part2_check state index 19690720 = index
day2part2_check state index result = day2part2_internal state (index+1)
