{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsState(..), IsQuestion(..), AnswerError(..), (?!), Answered, runAnswered, ContWithMessage(..), withMessage) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

newtype Answer = Answer { unAnswer :: String }

newtype AnswerError = AnswerError String

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

data ContWithMessage = ContWithMessage {
  contWith :: Cont,
  contMessage :: Maybe String
}

withoutMessage :: Cont -> ContWithMessage
withoutMessage c = ContWithMessage c Nothing

withMessage :: ContWithMessage -> String -> ContWithMessage
ContWithMessage c _ `withMessage` s = ContWithMessage c (Just s)

-- Answer -> IO (Either AnswerError c)
newtype Answered c = Answered {
    unAnswered :: ExceptT AnswerError IO c
  } deriving (Functor, Applicative, Monad, MonadError AnswerError, MonadIO)

runAnswered :: Answered c -> IO (Either AnswerError c)
runAnswered = runExceptT . unAnswered

(?!) :: Maybe a -> AnswerError -> Answered a -- E.ExceptT e m a
Nothing ?! e = throwError e
Just x  ?! _ = return x

infixr 3 ?!

-- | Continue in the same flow
cont :: IsState s => s -> ContWithMessage
cont i = withoutMessage $ Cont $ state i

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> s' -> ContWithMessage
start s s' = withoutMessage $ Start (state s) (state s')

-- | End the current flow
end :: IsState s => s -> ContWithMessage
end s = withoutMessage $ End (state s)

-- | Whether the state is a Question
class IsQuestion s where
  ask :: s -> Maybe String

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Answer -> Answered ContWithMessage,
  question :: Maybe String,
  save :: String
}


class (Read s, Show s, IsQuestion s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> Answer -> Answered ContWithMessage

  state :: s -> State
  state x = State {
      next = step x,
      question = ask x,
      save = show x
    }

instance Show State where
  show = save
