{-# LANGUAGE NoImplicitPrelude #-}

module Util
  (
    AOCApp(AOCApp, appName, appInputFile),
    runApp,
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
