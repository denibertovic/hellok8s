{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Hello.K8S.Api
  ( module Hello.K8S.Api.Definitions
  , module Hello.K8S.Api.Handlers
  , app
  ) where

import           Hello.K8S.Api.Definitions
import           Hello.K8S.Api.Handlers

import           Control.Monad.Reader          (runReaderT)
import           Network.Wai                   (Application)
import           Servant
import           Hello.K8S.Api.Definitions (AppM)
import           Hello.K8S.Config          (Config (..))
import           Hello.K8S.Types


helloApi :: Proxy HelloApi
helloApi = Proxy

readerToHandler :: Config -> AppM a -> Handler a
readerToHandler cfg app = runReaderT app cfg

readerToServer :: Config
               -> Server HelloApi
readerToServer cfg = hoistServer helloApi (readerToHandler cfg) helloServer

app :: Config -> Application
app cfg = serve helloApi (readerToServer cfg)

helloServer :: ServerT HelloApi AppM
helloServer = debug :<|> healthz :<|> hello
