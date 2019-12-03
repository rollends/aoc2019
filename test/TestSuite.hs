import Day1
import Day2

import Control.Concurrent
import Data.Int
import Data.List
import Text.Read
import Util

main :: IO ()
main = do
  putStr ""
  testDay1
--  testDay2_basic
  testDay2

testDay1 :: IO ()
testDay1 =
  let
    string_to_array = (map read) . lines
  in do
    file_content <- readFile "inputs/day1.in"
    putStr $ "Day 1: "
    print $ day1 (string_to_array file_content)

testDay2_basic :: IO ()
testDay2_basic =
  let
    commaToSpace = map (\a -> if a == ',' then ' ' else a)
    string_to_array = (map read) . words . commaToSpace
  in do
    file_content <- readFile "inputs/day2_basic.in"
    putStr $ "Day 2 (Basic): "
    print $ day2 (string_to_array file_content)

testDay2 :: IO ()
testDay2 =
  let
    commaToSpace = map (\a -> if a == ',' then ' ' else a)
    string_to_array = (map read) . words . commaToSpace
  in do
    file_content <- readFile "inputs/day2.in"
    putStr $ "Day 2: "
    print $ day2 (string_to_array file_content)
    putStr $ "Day 2 (Part 2): "
    print $ day2part2 (string_to_array file_content)
