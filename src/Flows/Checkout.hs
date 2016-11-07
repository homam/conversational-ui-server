{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Flows.Checkout (Suspended(Suspended), Stage(..), FinalResult) where

import FlowCont (Answer(..), IsState(..), Answered(..),
  cont, end,
  tell,
  IsFlow(..), deserialize)
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))

newtype BillingInfo = BillingInfo String deriving (Read, Show)

type FinalResult = BillingInfo

-- | Steps of this flow
data Stage a where
  AskBillingInfo :: Stage ()
  AskFinal       :: Stage FinalResult

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Note the namespace @Size@ (it's necessary for avoiding conflicts when reading @Stack@)
instance Show Suspended where
  show (Suspended stage as) = "Checkout.Suspended " ++ show stage ++ " " ++ show as

-- | Using the default implementation of 'readsPrecRP'
instance Read Suspended where
  readsPrec = readsPrecRP

-- | Is there a better way?
instance ReadParsec Suspended where
  readParsec = parseSuspended "Checkout" [
        read' "AskBillingInfo" AskBillingInfo
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name . Suspended

-- | 'step' function describes how the flow navigates from each step to the next
instance IsState Suspended where
  step (Suspended AskBillingInfo s) = cont
    "What is your credit card number?"
    (return . Suspended AskFinal . BillingInfo . unAnswer)

  step s@(Suspended AskFinal _) = end $ do
    tell "Your credit card info saved!"
    return s
