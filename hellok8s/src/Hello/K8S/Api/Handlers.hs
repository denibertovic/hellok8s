{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Hello.K8S.Api.Handlers where

import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Text                        as T
import           Lucid (Html, h2_, toHtml)
import           Network.HostName (getHostName)

import           Hello.K8S.Api.Definitions    (AppM)
import           Hello.K8S.Config             (Config (..))
import           Hello.K8S.Types
import           Hello.K8S.Utils

-- echo Handlers
hello :: AppM (Html ())
hello = return html
  where html :: Html ()
        html = do
          h2_ "Hi from k8s."

-- useful for demoing load balancing across pods in k8s
whoami :: AppM (Html ())
whoami = do
    hostname <- liftIO $ getHostName
    let txt = ("Hi from k8s. Hostname: " <> hostname <> ".")
    return $ html txt
  where html :: String -> Html ()
        html txt = do
          h2_ $ toHtml txt
