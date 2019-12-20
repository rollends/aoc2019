{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day5
import Util

import RIO
import qualified RIO.Text as T
import qualified Text.Read as TR

appDay5 :: RIO AOCApp ()
appDay5 =
  let
    commaToSpace = map (\a -> if a == ',' then ' ' else a)
    string_to_array = (map TR.read) . words . commaToSpace . T.unpack
  in do
    AOCApp _1 _2 inputFile <- ask
    file_content <- readFileUtf8 inputFile
    logInfo $ fromString "Part 1: "
    (logInfo . displayShow . (day5 1) . string_to_array) file_content
    logInfo $ fromString "Part 2: "
    (logInfo . displayShow . (day5 5) . string_to_array) file_content

main :: IO ()
main = runApp "Day 5" appDay5
