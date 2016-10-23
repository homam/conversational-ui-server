{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Flows.Checkout (Suspended(Suspended), Stage(..), FinalResult) where

import FlowCont (Answer(..), IsQuestion(..), IsState(..),
  cont, end, withMessage)
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))

newtype BillingInfo = BillingInfo String deriving (Read, Show)

type FinalResult = BillingInfo

data Stage a where
  AskBillingInfo :: Stage ()
  AskFinal       :: Stage FinalResult

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Checkout.Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended "Checkout" [
        read' "AskBillingInfo" AskBillingInfo
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name . Suspended

instance IsQuestion Suspended where
  ask (Suspended AskBillingInfo _) = Just "What is your credit card number?"
  ask (Suspended AskFinal       _) = Nothing

instance IsState Suspended where
  step (Suspended AskBillingInfo s) (Answer i) = return $
    end (Suspended AskFinal $ BillingInfo i) `withMessage` "Your billing info saved."
