{-# LANGUAGE GADTs, StandaloneDeriving
  , FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}

module Flows.TryAtHome (Stage(AskProduct), Suspended(Suspended), TryAtHomeResult) where

import FlowCont (Answer(..), IsQuestion(..), IsState(..),
  start, cont, end, intAnswer, withMessage, (>-*),
  IsFlow(..), deserialize)
import BiState ((:|:))
import Control.Applicative.Utils ((<.>))
import ParserUtil (parseSuspended, parseStage, ReadParsec(..))
import qualified Flows.Size as Size
import qualified Flows.Checkout as Checkout

import Control.Monad.IO.Class (liftIO)

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data TryAtHomeResult = TryAtHomeResult {
    tryAtHomeProduct :: Product
  , tryAtHomeSize :: Size.SizeResult
  , tryAtHomeAddress :: Address
  , tryAtHomeCheckout :: Checkout.FinalResult
} deriving (Show, Read)

toTryAthomeResult :: (Checkout.FinalResult, (Address, (Size.SizeResult, Product))) -> TryAtHomeResult
toTryAthomeResult (c, (a, (s, p))) = TryAtHomeResult {
    tryAtHomeProduct = p
  , tryAtHomeSize = s
  , tryAtHomeAddress = a
  , tryAtHomeCheckout = c
}

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

-- | Steps of this flow
data Stage a where
  AskProduct  :: Stage ()
  AskSize     :: Stage Product
  AskAddress  :: Stage (Size.SizeResult, Product)
  AskCheckout :: Stage (Address, (Size.SizeResult, Product))
  AskFinal    :: Stage TryAtHomeResult

deriving instance Show (Stage a)

-- | Note the namespace @TryAtHome@ (it's necessary for avoiding conflicts while reading the @Stack@)
instance Show Suspended where
  show (Suspended stage as) = "TryAtHome.Suspended " ++ show stage ++ " " ++ show as

-- | Using the default implementation of 'readsPrecRP'
instance Read Suspended where
  readsPrec = readsPrecRP

-- | Is there a better way?
instance ReadParsec Suspended where
  readParsec = parseSuspended "TryAtHome" [
        read' "AskSize"     AskSize
      , read' "AskProduct"  AskProduct
      , read' "AskAddress"  AskAddress
      , read' "AskCheckout" AskCheckout
      , read' "AskFinal"    AskFinal
    ]
    where
      read' name = parseStage name . Suspended

-- | TryAtHome is a main flow
instance IsFlow Suspended (Checkout.Suspended :|: Size.Suspended :|: Suspended) where
  deseralizeFlow (Suspended _ _) = deserialize

-- utility functions

-- | We might fetch the product from a DB
fetchProduct :: Int -> IO Product
fetchProduct = return . Product

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

-- | Questions that are presented to the user at each step of this flow
instance IsQuestion Suspended where
  ask (Suspended AskProduct  _) = Just "Which product do you want to buy?"
  ask (Suspended AskSize     _) = Nothing
  ask (Suspended AskAddress  _) = Just "What is your address?"
  ask (Suspended AskCheckout _) = Nothing
  ask (Suspended AskFinal    _) = Nothing

-- | 'step' function describes how the flow navigates from each step to the next
instance IsState Suspended where
  -- | Forks @Size@ flow
  step (Suspended AskProduct s) ans =
    let f pid = liftIO (Suspended AskSize <$> fetchProduct pid) >-* Size.Suspended Size.AskDoYou ()
    in intAnswer (flip withMessage "Thank you for choosing this product. Now select your size." <.> f) ans

  -- | Merges the result of @Size@ flow.
  -- This 'Answer' is the result of Size flow (that started in the previous step by 'Size.Suspended Size.AskDoYou ()').
  step (Suspended AskSize s) (Answer i) =
    let flowResult = read i :: Size.Suspended

    -- Logically the Answer can only be this instance: 'Size.Suspended Size.AskFinal Size.SizeResult'
    -- because this is the final result of the previous flow
    -- TODO: is there a better way of handling it?
    in case flowResult of
      Size.Suspended Size.AskFinal s' -> return $ cont $ Suspended AskAddress (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Size.Suspended Size.AskFinal s")

  -- | Forks @Checkout@ flow
  step (Suspended AskAddress s) (Answer i) = return (Suspended AskCheckout (Address i, s)) >-* Checkout.Suspended Checkout.AskBillingInfo ()

  -- | Merges the result of @Checkout@ flow
  step (Suspended AskCheckout s) (Answer i) =
    let flowResult = read i :: Checkout.Suspended
    in case flowResult of
      Checkout.Suspended Checkout.AskFinal s' -> return $
        end (Suspended AskFinal $ toTryAthomeResult (s', s)) `withMessage` "Tank you! You will reveive your item in 3 days."
      _ -> error ("error: " ++ i ++ " is not of type Checkout.Suspended Checkout.FinalResult s")
