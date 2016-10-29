{-# LANGUAGE TypeOperators #-}

module BiState  ((:|:)) where

import FlowCont (IsQuestion(..), IsState(..))

import Control.Arrow (first)
import Text.Read (Read(readsPrec))

-- | Union of states, to specify type of 'deserialize'
data l :|: r = LState l | RState r

infixr 5 :|:

instance (Read l, Read r, Show l, Show r) => Read (l :|: r) where
  readsPrec p s = map (first LState) (readsPrec p s) ++ map (first RState) (readsPrec p s)

instance (Show l, Show r) => Show (l :|: r) where
  show (LState x) = show x
  show (RState x) = show x

instance (IsQuestion l, IsQuestion r) => IsQuestion (l :|: r) where
  ask (LState x) = ask x
  ask (RState x) = ask x

instance (IsState l, IsState r) => IsState (l :|: r) where
  step (LState x) = step x
  step (RState x) = step x
