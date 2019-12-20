{-# LANGUAGE NoImplicitPrelude #-}

module Util
  (
    AOCApp(AOCApp, appName, appInputFile),
    runApp,
    uncurry3,
    intPow
  ) where

import RIO
import RIO.List.Partial((!!))
import System.Environment(getArgs)

data AOCApp = AOCApp { appName :: !Utf8Builder, appLogFunc :: !LogFunc, appInputFile :: String }

instance HasLogFunc AOCApp where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

runApp :: String-> RIO AOCApp a -> IO a
runApp name innerapp = do
  arguments <- getArgs
  loggingOptions <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True loggingOptions
  withLogFunc logOptions $ \loggingFunc -> do
    let app = AOCApp {
      appName = fromString name,
      appLogFunc = loggingFunc,
      appInputFile = arguments !! 0
    }
    runRIO app innerapp

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a1, b1, c1) = f a1 b1 c1

intPow :: Int -> Int -> Int
intPow b x =
  _intPow b x 1

_intPow b 0 v = v
_intPow b x v = _intPow b (x-1) (v*b)
