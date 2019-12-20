import Day2
import Day3
import Day4
import Day5
import Day6

import qualified RIO.Text as T

main :: IO ()
main = do
  day2_test1
  day3_test1
  day3_test2
  day3part2_test1
  day3part2_test2
  day4_test1
  day5part2_test1
  day5part2_test2
  day5part2_test3
  day6_test1

day2_test1 :: IO ()
day2_test1 = do
  putStrLn "\nDay 2 Test 1:"
  (putStrLn . show . (day2 9 10)) [1,9,10,3,2,3,11,0,99,30,40,50]
  putStrLn "Expected : 3500"

day3_test1 :: IO ()
day3_test1 = do
  putStrLn "\nDay 3 Test 1:"
  (putStrLn . show . day3) $ [
    ["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
    ["U62","R66","U55","R34","D71","R55","D58","R83"]]
  putStrLn "Expected : 159"

day3_test2 :: IO ()
day3_test2 = do
  putStrLn "\nDay 3 Test 2:"
  (putStrLn . show . day3) $ [
    ["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
    ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]]
  putStrLn "Expected : 135"

day3part2_test1 :: IO ()
day3part2_test1 = do
  putStrLn "\nDay 3 Part 2 Test 1:"
  (putStrLn . show . day3part2) $ [
    ["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
    ["U62","R66","U55","R34","D71","R55","D58","R83"]]
  putStrLn "Expected : 610"

day3part2_test2 :: IO ()
day3part2_test2 = do
  putStrLn "\nDay 3 Part 2 Test 2:"
  (putStrLn . show . day3part2) $ [
    ["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
    ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]]
  putStrLn "Expected : 410"

day5part2_test1 :: IO ()
day5part2_test1 = do
  putStrLn "\nDay 5 Part 2 Test 1:"
  (putStrLn . show . (day5 6)) $ [
    3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
  putStrLn "Expected : 999"

day5part2_test2 :: IO ()
day5part2_test2 = do
  putStrLn "\nDay 5 Part 2 Test 2:"
  (putStrLn . show . (day5 100)) $ [
    3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
  putStrLn "Expected : 1001"

day5part2_test3 :: IO ()
day5part2_test3 = do
  putStrLn "\nDay 5 Part 2 Test 3:"
  (putStrLn . show . (day5 8)) $ [
    3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
  putStrLn "Expected : 1000"

day4_test1 :: IO()
day4_test1 = do
  putStrLn "\nDay 4 Test 1:"
  (putStrLn . show) $ day4 125730 579381


day6_test1 :: IO ()
day6_test1 = do
  putStrLn "\nDay 6 Test 1:"
  (putStrLn . show . day6) $ [
    (T.pack "COM", T.pack "B"),
    (T.pack "B", T.pack "C"),
    (T.pack "C", T.pack "D"),
    (T.pack "D", T.pack "E"),
    (T.pack "E", T.pack "F"),
    (T.pack "B", T.pack "G"),
    (T.pack "G", T.pack "H"),
    (T.pack "D", T.pack "I"),
    (T.pack "E", T.pack "J"),
    (T.pack "J", T.pack "K"),
    (T.pack "K", T.pack "L")]
  putStrLn "Expected : 42"
