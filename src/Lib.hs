{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp, app) where

import GHC.Generics
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Aeson
import System.Process
import System.Exit
import Control.Exception (try, SomeException)

data CommandRequest = CommandRequest
  { command :: String
  } deriving (Eq, Show, Generic)

instance FromJSON CommandRequest

data CommandResponse = CommandResponse
  { output   :: Maybe Text
  , err      :: Maybe Text
  , exitCode :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON CommandResponse

type API =
       "run" :> ReqBody '[JSON] CommandRequest
             :> Post '[JSON] CommandResponse

server :: Server API
server = runCommand

runCommand :: CommandRequest -> Handler CommandResponse
runCommand (CommandRequest cmd) = do
  result <- liftIO $ try (readCreateProcessWithExitCode (shell cmd) "") :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left ex -> pure $ CommandResponse Nothing (Just $ pack $ show ex) 1
    Right (code, out, err') ->
      pure $ CommandResponse
        { output = if null out then Nothing else Just $ pack out
        , err = if null err' then Nothing else Just $ pack err'
        , exitCode = case code of
            ExitSuccess   -> 0
            ExitFailure n -> n
        }

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: Int -> IO ()
startApp port = do
  putStrLn "🚀 Running on http://localhost:" ++ show port
  run port app
