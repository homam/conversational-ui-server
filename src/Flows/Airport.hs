{-# LANGUAGE GADTs, StandaloneDeriving, TupleSections
  , FlexibleInstances, MultiParamTypeClasses #-}

module Flows.Airport (Suspended(Suspended), AirportResult(..), Itinerary(..), City(..), Stage(AskCity, AskFinal)) where

import FlowCont (Answer(..), Cont(..), IsState(..),
  cont, end, yesNoAnswer, intAnswer, selectAnswer,
  validateAnswer_, throwAnswerError,
  tell,
  IsFlow(..), deserialize)
import ParserUtil (parseSuspended, parseStage, ReadParsec(readsPrecRP, readParsec))

import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO)

newtype City = City { unCity :: String } deriving (Show, Read)
data Itinerary = Origin | Destination deriving (Show, Read)

data AirportResult = AirportResult {
  airportItinerary :: Itinerary,
  airportCity :: City
} deriving (Show, Read)

data Stage a where
  AskCity    :: Stage Itinerary
  AskAirport :: Stage ([City], Itinerary)
  AskConfirm :: Stage (City, Itinerary)
  AskFinal   :: Stage AirportResult

deriving instance Show (Stage a)

-- | Size is a main Flow
instance IsFlow Suspended Suspended where
  deseralizeFlow (Suspended _ _) = deserialize

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Note the namespace @TryAtHome@ (it's necessary for avoiding conflicts while reading the @Stack@)
instance Show Suspended where
  show (Suspended stage as) = "Airport.Suspended " ++ show stage ++ " " ++ show as

-- | Using the default implementation of 'readsPrecRP'
instance Read Suspended where
  readsPrec = readsPrecRP

-- | Is there a better way?
instance ReadParsec Suspended where
  readParsec = parseSuspended "Airport" [
        read' "AskCity"      AskCity
      , read' "AskAirport" AskAirport
      , read' "AskConfirm"   AskConfirm
      , read' "AskFinal"     AskFinal
    ]
    where
      read' name = parseStage name . Suspended

fetchCity :: String -> IO [City]
fetchCity "Dubai" = return [City "DXB", City "DWC"]
fetchCity "Amsterdam" = return [City "AMS"]
fetchCity s = return []

citiesToString cities = foldl1 (++) (intersperse " or " (map unCity cities))

-- | 'step' function describes how the flow navigates from each step to the next
instance IsState Suspended where
  step (Suspended AskCity s) = cont ("Where are you flying " ++ itin' ++ "?") (\ (Answer i) -> next =<< liftIO (fetchCity i))
    where
      next ls@(h:n:_) = return $ Suspended AskAirport (ls, s)
      next   [h]      =  return $ Suspended AskConfirm (h, s)
      next    _       = throwAnswerError "No airport found"

      itin' = case s of
        Origin -> "from"
        Destination -> "to"

  step (Suspended AskAirport (cities, itin)) = cont
    ("Select one airport " ++ citiesToString cities)
    (selectAnswer
      ("Please answer with either " ++ citiesToString cities)
      (map (\ city@(City c) -> ([c], return $ Suspended AskConfirm (city, itin))) cities)
    )

  step (Suspended AskConfirm (city, itin)) = cont
    "Please confirm you selection."
    (yesNoAnswer
      (return $ Suspended AskFinal $ AirportResult itin city)
      (return $ Suspended AskCity itin)  -- `withMessage` "Select another city.")
    )
  --   ans
  --
  step p@(Suspended AskFinal _) = end $ do
    tell ["You selected your Airport"]
    return p
