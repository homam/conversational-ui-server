{-# LANGUAGE GADTs, StandaloneDeriving, TupleSections
  , FlexibleInstances, MultiParamTypeClasses #-}

module Flows.Date (Suspended(Suspended), Stage(AskInit, AskFinal)) where

import FlowCont (Answer(..), Cont(..), IsState(..), Answered(..),
  cont, end, yesNoAnswer, intAnswer, selectAnswer, fetchAnswers,
  validateAnswer_, throwAnswerError, readAnswerF, oneOf, matchAnswerLower,
  tell,
  IsFlow(..), deserialize)
import ParserUtil (parseSuspended, parseStage, ReadParsec(readsPrecRP, readParsec))

import Data.Time (parseTimeM, defaultTimeLocale, UTCTime, getModJulianDate, getCurrentTime, addUTCTime,
  formatTime, FormatTime)
import Control.Monad (msum, join)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow ((***), (&&&))

newtype City = City { unCity :: String } deriving (Show, Read)
data Itinerary = Origin | Destination deriving (Show, Read)

data AirportResult = AirportResult {
  airportItinerary :: Itinerary,
  airportCity :: City
} deriving (Show, Read)

data Stage a where
  AskInit     :: Stage ()
  AskOutbound :: Stage UTCTime
  AskFinal    :: Stage (Maybe UTCTime, UTCTime)

deriving instance Show (Stage a)

-- | Size is a main Flow
instance IsFlow Suspended Suspended where
  deseralizeFlow (Suspended _ _) = deserialize

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Note the namespace @Date@ (it's necessary for avoiding conflicts while reading the @Stack@)
instance Show Suspended where
  show (Suspended stage as) = "Date.Suspended " ++ show stage ++ " " ++ show as

-- | Using the default implementation of 'readsPrecRP'
instance Read Suspended where
  readsPrec = readsPrecRP

-- | Is there a better way?
instance ReadParsec Suspended where
  readParsec = parseSuspended "Date" [
        read' "AskInit"     AskInit
      , read' "AskOutbound" AskOutbound
      , read' "AskFinal"    AskFinal
    ]
    where
      read' name = parseStage name . Suspended


instance IsState Suspended where
  step (Suspended AskInit ()) = cont
    "When do you want to fly?"
    (readAnswerF
      parseDate
      "Please enter a valid date"
      ( \ d -> do
        range <- (&&&) id (addDays 365) <$> liftIO getCurrentTime
        validateDate range (return . Suspended AskOutbound) d
      )
    )

  step (Suspended AskOutbound outbound) = cont
    "When do you want to return? Enter a date or type oneway."
    (oneOf
      "Please either enter a date or \"oneway\" if you like to book a oneway ticket."
      [
          matchAnswerLower [(["one way", "oneway"], return $ Suspended AskFinal (Nothing, outbound))]
        , fmap (
            validateDate range (return . Suspended AskFinal . (, outbound) . Just)
          ) . parseDate . unAnswer
      ]
    )
    where
      range = (&&&) (addDays 1) (addDays 365) outbound

  step s@(Suspended AskFinal _) = end (return s)


validateDate :: (UTCTime, UTCTime) -> (UTCTime -> Answered a) -> UTCTime -> Answered a
validateDate (lower, upper) fc d =
  if d <= lower || d >= upper
  then throwAnswerError $ "Please select a date between " ++ formatD lower ++ " and " ++ formatD upper
  else fc d

parseDate :: String -> Maybe UTCTime
parseDate s = msum $ map (\ f -> parseTimeM True defaultTimeLocale f s) [
    "%Y-%m-%d",
    "%Y %b %d", -- "%Y %b %e",
    "%d %b %Y", "%e %b %Y",
    "%b %d %Y", "%b %e %Y"
  ]
  -- %b month name
  -- %e day of month, space-padded to two chars, 1 - 31
  -- %d day of month, 0-padded to two chars, 01 - 31

formatD ::FormatTime t => t -> String
formatD = formatTime defaultTimeLocale "%Y-%m-%d"

addDays :: Int -> UTCTime -> UTCTime
addDays = addUTCTime . realToFrac . (3600 * 24 *)
