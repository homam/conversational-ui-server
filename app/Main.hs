{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Flow

import qualified Web.Scotty as S
import qualified Web.Scotty.Trans as ST
import Network.HTTP.Types (status404)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import Data.Text (Text)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Control.Exception.Base (catch, evaluate, SomeException)
import qualified Data.Text.Internal.Lazy as LT
import Debug.Trace (trace)

t s = fromString s :: Text

defaultHandler :: LT.Text -> ST.ActionT LT.Text IO ()
defaultHandler _ = S.status status404

instance JSON.ToJSON Flow.StepResult where
  toJSON = JSON.genericToJSON (JSON.defaultOptions { omitNothingFields = True })

server :: S.ScottyM ()
server = do

  S.defaultHandler defaultHandler

  S.post "/" $ do
      answer <- S.param "answer"
      stack  <- S.param "stack" `S.rescue` const (return "[]")
      res <- liftIO (Flow.receiveAnswer stack answer)
      S.json res

      -- (question, stack') <- liftIO $ catch (Flow.receiveAnswer stack answer) handler
      -- S.json $ JSON.object [ "question" .= t (fromMaybe "" question), "stack" .= t stack' ]
      -- where
      --   handler :: SomeException -> IO (Maybe String, String)
      --   handler x = trace (show x) $ return (Nothing, "ERROR")

main :: IO ()
-- main = Flow.main
-- Flow.receiveAnswer "[]" "124"
main = S.scotty 9176 server
