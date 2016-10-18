{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Checkout (Suspended(Suspended), Stage(..), FinalResult) where

import FlowCont (Answer(..), Cont(..), IsQuestion(..), IsState(..), start, cont, end)
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))

newtype BillingInfo = BillingInfo String deriving (Read, Show)

type FinalResult = (BillingInfo, ())

data Stage a where
  AskCheckoutBillingInfo :: Stage ()
  AskFinal       :: Stage (BillingInfo, ())

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Checkout.Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended "Checkout" [
        read' "AskCheckoutBillingInfo" AskCheckoutBillingInfo
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name  . Suspended

instance IsQuestion Suspended where
  ask (Suspended AskCheckoutBillingInfo _) = Just "What is your credit card number?"
  ask (Suspended AskFinal       _) = Nothing

instance IsState Suspended where
  step (Suspended AskCheckoutBillingInfo s) (Answer i) = end $ Suspended AskFinal (BillingInfo i, s)
