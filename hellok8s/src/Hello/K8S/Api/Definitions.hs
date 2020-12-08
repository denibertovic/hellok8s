{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Hello.K8S.Api.Definitions where

import           Control.Monad.Reader        (ReaderT)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Lucid (Html)
import           Hello.K8S.Config        (Config (..))



type HelloApi = "who-are-you" :> Get '[HTML] (Html ())
                    :<|> Get '[HTML] (Html ())

type AppM = ReaderT Config Handler
