{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import System.Environment (getArgs, getEnvironment)

import Hello.K8S.Api (app)
import Hello.K8S.Config (Config(..), Environment(..), mkConfig, setLogger)
import Hello.K8S.Utils (lookupSetting)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Launching..." >> inner
    ["check"] -> putStrLn "Run successful"
    _ -> error "Invalid arguments, use either nothing or the word check"

inner :: IO ()
inner = do
  env <- getEnvironment
  config <- mkConfig env
  let logger = setLogger (environment config)
  let settings = setPort (appPort config) $
                 defaultSettings
  runSettings settings $ logger $ app config
