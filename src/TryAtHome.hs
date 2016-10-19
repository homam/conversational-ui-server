{-# LANGUAGE GADTs, StandaloneDeriving #-}

module TryAtHome (Stage(AskProduct, AskFinal), Suspended(Suspended)) where

import FlowCont (Answer(..), Cont(..), IsQuestion(..), IsState(..), start, cont, end)
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))
import qualified Size
import qualified Checkout
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "TryAtHome.Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended "TryAtHome" [
        read' "AskSize"     AskSize
      , read' "AskProduct"  AskProduct
      , read' "AskAddress"  AskAddress
      , read' "AskCheckout" AskCheckout
      , read' "AskFinal"    AskFinal
    ]
    where
      read' name = parseStage name  . Suspended

data Stage a where
  AskProduct  :: Stage ()
  AskSize     :: Stage (Product, ())
  AskAddress  :: Stage (Size.SizeResult, (Product, ()))
  AskCheckout :: Stage (Address, (Size.SizeResult, (Product, ())))
  AskFinal    :: Stage (Checkout.FinalResult, (Address, (Size.SizeResult, (Product, ()))))

deriving instance Show (Stage a)


getProduct :: s -> Int -> (Product, s)
getProduct s i = (Product i, s)

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

instance IsQuestion Suspended where
  ask (Suspended AskProduct  _) = Just "Which product do you want to buy?"
  ask (Suspended AskSize     _) = Nothing
  ask (Suspended AskAddress  _) = Just "What is your address?"
  ask (Suspended AskCheckout _) = Nothing
  ask (Suspended AskFinal    _) = Nothing

instance IsState Suspended where
  step current@(Suspended AskProduct s) (Answer i) =
    let f pid = start (Suspended AskSize (getProduct s pid)) (Size.Suspended Size.AskDoYou ())
    in  fromMaybe (cont current) (f <$> readMaybe i)
  step (Suspended AskSize s) (Answer i) =
    let flowResult = read i :: Size.Suspended
    in case flowResult of
      Size.Suspended Size.AskFinal s' -> cont $ Suspended AskAddress (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Size.Suspended Size.AskFinal s")
  step (Suspended AskAddress s) (Answer i) = start (Suspended AskCheckout (Address i, s)) (Checkout.Suspended Checkout.AskCheckoutBillingInfo ())
  step (Suspended AskCheckout s) (Answer i) =
    let flowResult = read i :: Checkout.Suspended
    in case flowResult of
      Checkout.Suspended Checkout.AskFinal s' -> end $ Suspended AskFinal (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Checkout.Suspended Checkout.FinalResult s")
