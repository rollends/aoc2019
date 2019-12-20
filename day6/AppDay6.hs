{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day6
import Util

import RIO
import qualified RIO.Text as T

appDay6 :: RIO AOCApp ()
appDay6 =
  let
    toListOfPairs = (map tokenize) . (T.lines)
  in do
    AOCApp _1 _2 inputFile <- ask
    file_content <- readFileUtf8 inputFile
    logInfo $ fromString "Part 1: "
    (logInfo . displayShow . day6 . toListOfPairs) file_content
    logInfo $ fromString "Part 2: "
    (logInfo . displayShow . day6part2 . toListOfPairs) file_content

tokenize :: Text -> (Text, Text)
tokenize line =
  let
    (parent, childWithParen) = T.break (\c -> c == ')') line
  in
    (T.strip parent, T.strip . (T.drop 1) $ childWithParen)

main :: IO ()
main = runApp "Day 6" appDay6
