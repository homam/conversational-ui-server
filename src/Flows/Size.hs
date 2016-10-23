{-# LANGUAGE GADTs, StandaloneDeriving
  , FlexibleInstances, MultiParamTypeClasses #-}

module Flows.Size (Suspended(Suspended), SizeResult, Stage(AskDoYou, AskFinal), Size(..), Weight(..), Height(..)) where

import FlowCont (Answer(..), Cont(..), IsQuestion(..), IsState(..),
  cont, end, yesNoAnswer, intAnswer, withMessage,
  IsFlow(..), deserialize)
import ParserUtil (parseSuspended, parseStage, ReadParsec(readsPrecRP, readParsec))
import Text.Read (Read(readsPrec))

newtype Size = Size Int deriving (Read, Show)
newtype Height = Height Int deriving (Read, Show)
newtype Weight = Weight Int deriving (Read, Show)

data SizeResult = SizeResult {
  knownSize :: Bool,
  size :: Size
} deriving (Read, Show)

data Stage a where
  AskDoYou  :: Stage ()
  AskSize   :: Stage (Bool, ())
  AskWeight :: Stage (Bool, ())
  AskHeight :: Stage (Weight, (Bool, ()))
  AskFinal  :: Stage SizeResult

deriving instance Show (Stage a)

instance IsFlow Suspended Suspended where
  deseralizeFlow (Suspended _ _) = deserialize

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Size.Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended "Size" [
        read' "AskSize"   AskSize
      , read' "AskDoYou"  AskDoYou
      , read' "AskWeight" AskWeight
      , read' "AskHeight" AskHeight
      , read' "AskFinal"  AskFinal
    ]
    where
      read' name = parseStage name . Suspended

getKnownSize :: (Bool, s) -> Int -> SizeResult
getKnownSize (b, _) i = SizeResult { size = Size i, knownSize = b}

getWeight :: s -> Int -> (Weight, s)
getWeight s i = (Weight i, s)

getHeight :: (Weight, (Bool, s)) -> Int -> SizeResult
getHeight (Weight w, (b, _)) i = SizeResult { size = Size $ w * i, knownSize = b }

instance IsQuestion Suspended where
  ask (Suspended AskDoYou  _) = Just "Do you know your size?"
  ask (Suspended AskWeight _) = Just "What is your weight?"
  ask (Suspended AskHeight _) = Just "What is your height?"
  ask (Suspended AskSize   _) = Just "What is your size?"
  ask (Suspended AskFinal  _) = Nothing

instance IsState Suspended where
  step (Suspended AskDoYou s) = yesNoAnswer
    (return $ cont $ Suspended AskSize (True, s))
    (return $ cont $ Suspended AskWeight (False, s))

  step (Suspended AskWeight s) = intAnswer
    (return . cont . Suspended AskHeight . getWeight s)

  step (Suspended AskHeight s) = intAnswer
    (\ i ->
      let size = getHeight s i
      in  return $ end (Suspended AskFinal size) `withMessage` ("Your size is " ++ show size)
    )

  step (Suspended AskSize s) = intAnswer
    (return . end . Suspended AskFinal . getKnownSize s)

  step (Suspended AskFinal  _) = const $ error "Flow already ended."
