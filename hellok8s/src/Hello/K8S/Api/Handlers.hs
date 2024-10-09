{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Hello.K8S.Api.Handlers where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (asks, ask)
import qualified Data.Text                        as T
import           Lucid (Html, h2_, p_, hr_, div_, toHtml)
import           Network.HostName (getHostName)
import           Servant

import           Hello.K8S.Api.Definitions    (AppM)
import           Hello.K8S.Config             (Config (..))
import           Hello.K8S.Types
import           Hello.K8S.Utils

-- echo Handlers
hello :: AppM (Html ())
hello = do
    simulateError <- asks simulateCrashLoop
    if simulateError then (throwError err500 {errBody = "Simulating 500 Error"}) else (return html)
  where html :: Html ()
        html = do
          h2_ "Hi from k8s."

-- useful for demoing load balancing across pods in k8s
debug :: AppM (Html ())
debug = do
    simulateError <- asks simulateCrashLoop
    config <- ask
    hostname <- liftIO $ getHostName
    let txt = ("The HTTP Response came from: " <> hostname)
    if simulateError then (throwError err500 {errBody = "Simulating 500 Error"}) else (return $ html txt (show config))
  where html :: String -> String -> Html ()
        html txt debugInfo = do
          h2_ $ toHtml txt
          hr_ []
          p_ $ toHtml debugInfo

healthz :: AppM (Html ())
healthz = do
    simulateError <- asks simulateCrashLoop
    if simulateError then (throwError err500 {errBody = "not ok!"}) else (return $ toHtml "ok")
