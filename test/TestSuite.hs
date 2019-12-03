import Day1
import Util
import Data.Int
import Text.Read

main :: IO ()
main = do
  testDay1

testDay1 :: IO ()
testDay1 = 
  let
    string_to_array = (map read) . lines
  in do
    file_content <- readFile "inputs/day1.in"
    print $ day1 (string_to_array file_content)
