{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsState(..), IsQuestion(..), AnswerError(..), (?!)) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Identity

newtype Answer = Answer { unAnswer :: String }

newtype AnswerError = AnswerError { unAnswerError :: (String, State) }

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

-- Answer -> IO (Either AnswerError c)
newtype Answered c = Answered { runAnswered :: ExceptT AnswerError Identity c  } deriving (Functor, Applicative, Monad)

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left  e
Just x  ?! _ = Right x

-- | Continue in the same flow
cont :: IsState s => s -> IO (Either AnswerError Cont)
cont i = Right . Cont <$> state i

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> s' -> IO (Either AnswerError Cont)
start s s' = Right <$> (Start <$> state s <*> state s')

-- | End the current flow
end :: IsState s => s -> IO (Either AnswerError Cont)
end s = Right . End <$> state s

-- | Whether the state is a Question
class IsQuestion s where
  ask :: s -> Maybe String

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Answer -> IO (Either AnswerError Cont),
  question :: Maybe String,
  save :: String
}


class (Read s, Show s, IsQuestion s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> Answer -> IO (Either AnswerError Cont)

  state :: s -> IO State
  state x = return State {
      next = step x,
      question = ask x,
      save = show x
    }

instance Show State where
  show = save
