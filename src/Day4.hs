{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( day4
  ) where

import RIO
import RIO.List as L
import RIO.List.Partial as L'

day4 :: Int -> Int -> Int
day4 start end =
  _day4 start end 0

_day4 start end count =
  if start > end then
    count
  else
    if isValidPassword (show start) then
      _day4 (start + 1) end (count + 1)
    else
      _day4 (start + 1) end count

isValidPassword :: String -> Bool
isValidPassword password =
  if length password == 6 then
    checkRules password
  else
    False

rule3 password =
  let
    histogram = map (\x -> length (findIndices ((==) x) password)) ['1','2','3','4','5','6','7','8','9']
  in
    any (\bin -> bin == 2) histogram

checkRules password =
  let
    pairs = zip password (L'.tail password)
    rule4 = all (\(a,b) -> a <= b)
  in
    (rule3 password) && (rule4 pairs)
