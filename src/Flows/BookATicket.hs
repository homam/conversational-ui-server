{-# LANGUAGE GADTs, StandaloneDeriving
  , FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}

module Flows.BookATicket (Suspended(..), Stage(AskInit)) where

import FlowCont (Answer(..), IsQuestion(..), IsState(..),
  start, cont, end, intAnswer, withMessage, (>-*),
  IsFlow(..), deserialize)
import BiState ((:|:))
import Control.Applicative.Utils ((<.>))
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))
import qualified Flows.Airport as Airport

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Steps of this flow
data Stage a where
  AskInit        :: Stage ()
  AskOrigin      :: Stage ()
  AskDestination :: Stage Airport.City
  AskWhen        :: Stage (Airport.City, Airport.City)
  -- AskCheckout :: Stage (Address, (Size.SizeResult, Product))
  -- AskFinal    :: Stage BookATicketResult

deriving instance Show (Stage a)

-- | Note the namespace @BookATicket@ (it's necessary for avoiding conflicts while reading the @Stack@)
instance Show Suspended where
  show (Suspended stage as) = "BookATicket.Suspended " ++ show stage ++ " " ++ show as

-- | Using the default implementation of 'readsPrecRP'
instance Read Suspended where
  readsPrec = readsPrecRP

-- | Is there a better way?
instance ReadParsec Suspended where
  readParsec = parseSuspended "BookATicket" [
        read' "AskInit"        AskInit
      , read' "AskOrigin"      AskOrigin
      , read' "AskDestination" AskDestination
      , read' "AskWhen"        AskWhen
    ]
    where
      read' name = parseStage name . Suspended

-- | BookATicket is a main flow
instance IsFlow Suspended (Airport.Suspended :|: Suspended) where
  deseralizeFlow (Suspended _ _) = deserialize

-- | Questions that are presented to the user at each step of this flow
instance IsQuestion Suspended where
  ask (Suspended AskInit        _) = Nothing
  ask (Suspended AskOrigin      _) = Nothing
  ask (Suspended AskDestination _) = Nothing
  ask (Suspended AskWhen        _) = Just "When do you want to fly?"

-- | 'step' function describes how the flow navigates from each step to the next
instance IsState Suspended where
  step (Suspended AskInit ()) _ = return (Suspended AskOrigin ()) >-* Airport.Suspended Airport.AskCity Airport.Origin
  step (Suspended AskOrigin ()) (Answer i) =
    let flowResult = read i :: Airport.Suspended
    in case flowResult of
      Airport.Suspended Airport.AskFinal s' ->
        return (Suspended AskDestination (Airport.airportCity s')) >-* Airport.Suspended Airport.AskCity Airport.Destination
      _ -> error ("error: " ++ i ++ " is not of type Airport.Suspended Airport.AskFinal s")
