import Day2
import Day3
import Day4

main :: IO ()
main = do
  day2_test1
  day3_test1
  day3_test2
  day3part2_test1
  day3part2_test2
  day4_test1

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

day4_test1 :: IO()
day4_test1 = do
  putStrLn "\nDay 4 Test 1:"
  (putStrLn . show) $ day4 125730 579381

