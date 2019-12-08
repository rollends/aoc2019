{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day3
import Util

import RIO
import qualified RIO.Text as T

appDay3 :: RIO AOCApp ()
appDay3 =
  let
    commaToSpace = map (\a -> if a == ',' then ' ' else a)
    string_to_array = words . commaToSpace
  in do
    AOCApp _ _ inputFile <- ask
    file_content <- readFileUtf8 inputFile
    logInfo $ fromString "Part 1: "
    logInfo . displayShow $
      day3 (map string_to_array (lines . T.unpack $ file_content))
    logInfo $ fromString "Part 2: "
    logInfo . displayShow $
      day3part2 (map string_to_array (lines . T.unpack $ file_content))

main :: IO ()
main = runApp "Day 3" appDay3
