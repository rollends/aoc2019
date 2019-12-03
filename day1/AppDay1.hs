{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day1
import Util

import RIO
import qualified RIO.Text as T
import qualified Text.Read as TR

main :: IO ()
main = runApp "Day 1" appDay1

appDay1 :: RIO AOCApp ()
appDay1 =
  let
    string_to_array = (map $ TR.read . T.unpack) . T.linesCR
  in do
    AOCApp _1 _2 inputFile <- ask
    file_content <- readFileUtf8 inputFile
    (logInfo . displayShow . day1 . string_to_array) file_content


