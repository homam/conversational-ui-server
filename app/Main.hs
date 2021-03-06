{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Main where

import qualified Flows

import qualified Web.Scotty as S
import qualified Web.Scotty.Trans as ST
import Network.HTTP.Types (status502)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Data.Aeson.Types
import Data.Text (Text)
import Data.String (fromString)
import Control.Exception.Base (catch, SomeException)
import qualified Data.Text.Internal.Lazy as LT
import Debug.Trace (trace)

deriving instance Generic Flows.StepResult

instance JSON.ToJSON Flows.StepResult where
  toJSON = JSON.genericToJSON (JSON.defaultOptions { omitNothingFields = True })

t s = fromString s :: Text

defaultHandler :: LT.Text -> ST.ActionT LT.Text IO ()
defaultHandler x = do
  S.status status502
  S.json $ JSON.object [ "exception" .= x ]

server :: S.ScottyM ()
server = do

  S.defaultHandler defaultHandler

  S.post "/" $ do
      answer <- S.param "answer"
      stack  <- S.param "stack" `S.rescue` const (return "[]")
      res <- liftIO $ catch (Right <$> Flows.receiveAnswer Flows.TryAtHome stack answer) handler
      either (S.raise . fromString) S.json res

      where
        handler :: SomeException -> IO (Either String Flows.StepResult)
        handler = return . Left . show

main :: IO ()
-- main = Flow.main
-- Flow.receiveAnswer "[]" "124"
main = S.scotty 9176 server
