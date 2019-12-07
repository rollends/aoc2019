{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day2
import Util

import RIO
import qualified RIO.Text as T
import qualified Text.Read as TR

appDay2 :: RIO AOCApp ()
appDay2 =
  let
    commaToSpace = map (\a -> if a == ',' then ' ' else a)
    string_to_array = (map TR.read) . words . commaToSpace . T.unpack
  in do
    AOCApp _1 _2 inputFile <- ask
    file_content <- readFileUtf8 inputFile
    logInfo $ fromString "Part 1: "
    (logInfo . displayShow . (day2 12 2) . string_to_array) file_content
    logInfo $ fromString "Part 2: "
    (logInfo . displayShow . (day2part2 19690720) . string_to_array) file_content

main :: IO ()
main = runApp "Day 2" appDay2
