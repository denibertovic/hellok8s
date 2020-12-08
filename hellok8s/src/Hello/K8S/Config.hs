{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hello.K8S.Config where

import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Network.Wai                          (Middleware)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Data.HashMap.Strict as HM


data Config = Config
    { environment  :: Environment
    , appPort :: Int
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

mkConfig :: [(String, String)] -> IO Config
mkConfig env = do
  return $ Config { environment = getEnv
             , appPort = requiredDefault 8000 "APP_PORT" hm
             }
    where hm = HM.fromList env
          getEnv = requiredDefault Development "ENV" hm

notRequired :: Text -> HM.HashMap String String -> Maybe Text
notRequired q env = T.pack <$> HM.lookup (T.unpack q) env

required :: Text -> HM.HashMap String String -> Text
required q env = case HM.lookup (T.unpack q) env of
  Nothing -> error $ "Error. Missing config environment variable: " <> (T.unpack q)
  Just x -> T.pack x

requiredDefault :: Read a => a -> Text -> HM.HashMap String String -> a
requiredDefault def q env = case HM.lookup (T.unpack q) env of
  Nothing -> def
  Just x -> read x

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout
