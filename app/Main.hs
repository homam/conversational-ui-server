{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Flow

import qualified Web.Scotty as S
import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.String (fromString)
import Data.Maybe (fromMaybe)

t s = fromString s :: Text

server :: S.ScottyM ()
server =
  S.post "/" $ do
      answer <- S.param "answer" :: S.ActionM String
      stack  <- S.param "stack"  :: S.ActionM String
      (question, stack') <- S.liftAndCatchIO $ Flow.receiveAnswer stack answer
      S.json $ JSON.object [ "question" .= t (fromMaybe "" question), "stack" .= t stack' ]

main :: IO ()
-- main = Flow.main
-- Flow.receiveAnswer "[]" "124"
main = S.scotty 9176 server
