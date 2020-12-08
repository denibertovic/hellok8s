{-# LANGUAGE OverloadedStrings #-}

module Hello.K8S.Types where

import           Data.Aeson             (ToJSON, object, toJSON, (.=))

newtype JsonError =
  JsonError String

instance ToJSON JsonError where
  toJSON (JsonError b) = object ["error" .= b]
