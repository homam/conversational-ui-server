{-# LANGUAGE GADTs, StandaloneDeriving
  , FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}

module Flows.BookATicket (Suspended(..), Stage(AskOrigin)) where

import FlowCont (Answer(..), IsState(..),
  start, cont, end, intAnswer,
  IsFlow(..), deserialize,
  Answered(..),
  tell)
import BiState ((:|:))
import Control.Applicative.Utils ((<.>))
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))
import qualified Flows.Airport as Airport
import qualified Flows.Date as Date

-- import Data.Time (parseTimeM, defaultTimeLocale)
-- import Control.Monad (msum)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Steps of this flow
data Stage a where
  AskOrigin         :: Stage ()
  AskDestination    :: Stage Airport.City
  AskDate           :: Stage (Airport.City, Airport.City)
  -- AskHowMany        :: Stage (Airport.City, Airport.City)
  AskFinal          :: Stage (Date.Suspended, (Airport.City, Airport.City))
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
        read' "AskOrigin"         AskOrigin
      , read' "AskDestination"    AskDestination
      , read' "AskDate"           AskDate
      , read' "AskFinal"          AskFinal
    ]
    where
      read' name = parseStage name . Suspended

-- | BookATicket is a main flow
instance IsFlow Suspended (Date.Suspended :|: Airport.Suspended :|: Suspended) where
  deseralizeFlow (Suspended _ _) = deserialize

-- | 'step' function describes how the flow navigates from each step to the next
instance IsState Suspended where
  step (Suspended AskOrigin ()) = start
    (Airport.Suspended Airport.AskCity Airport.Origin :: Airport.Suspended)
    ((\ (Airport.Suspended Airport.AskFinal (Airport.AirportResult itin city)) -> do
        tell $ "C) Thanks for selecting the origin " ++ Airport.unCity city
        return $ Suspended AskDestination city
    ) :: Airport.Suspended -> Answered Suspended)

  step (Suspended AskDestination origin) = start
    (Airport.Suspended Airport.AskCity Airport.Destination :: Airport.Suspended)
    ((\ (Airport.Suspended Airport.AskFinal (Airport.AirportResult itin city)) -> do
        tell $ "C) Thanks for selecting the destination " ++ Airport.unCity city
        return $ Suspended AskDate (origin, city)
    ) :: Airport.Suspended -> Answered Suspended)


  step (Suspended AskDate s) = start
    (Date.Suspended Date.AskInit () :: Date.Suspended)
    ((\ p@(Date.Suspended Date.AskFinal s') -> return $ Suspended AskFinal (p, s)

    ) :: Date.Suspended -> Answered Suspended)

  -- step (Suspended AskHowMany s) = cont
  --   "How many tickets?"
  --   (intAnswer (\ i -> return $ Suspended AskFinal (i, s)))

  step p@(Suspended AskFinal _) = end $ do
    tell "Done for now"
    tell "More to come ..."
    return p
