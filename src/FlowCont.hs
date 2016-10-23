{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module FlowCont (Answer(..),
  Cont(..), cont, start, end, readAnswer, intAnswer, selectAnswer, yesNoAnswer,
  (>-*), (<.>), State(..), IsState(..), IsQuestion(..),
  AnswerError(..), throwAnswerError, Answered, runAnswered, ContWithMessage(..), withMessage) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Data.Char (toLower)


-- newtype Answer = Answer { unAnswer :: String }

newtype Answer a = Answer { unAnswer :: a } deriving Functor

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

throwAnswerError :: String -> Answered a
throwAnswerError = throwError . AnswerError

-- | Continue in the same flow
cont :: IsState s => s -> ContWithMessage
cont = withoutMessage . Cont . state

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> s' -> ContWithMessage
start s s' = withoutMessage $ Start (state s) (state s')

(>-*) :: (IsState s, IsState s') => Answered s -> s' -> Answered ContWithMessage
ms >-* s' = flip start s'<$> ms
infixr 5 >-*

-- | End the current flow
end :: IsState s => s -> ContWithMessage
end = withoutMessage . End . state

-- | Whether the state is a Question
class IsQuestion s where
  ask :: s -> Maybe String

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Answer String -> Answered ContWithMessage,
  question :: Maybe String,
  save :: String
}


class (Read s, Show s, IsQuestion s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> Answer String -> Answered ContWithMessage

  state :: s -> State
  state x = State {
      next = step x,
      question = ask x,
      save = show x
    }

instance Show State where
  show = save


-- answer processing helper functions

validateAnswer :: (String -> Either String b) -> (b -> Answered a) -> Answer String -> Answered a
validateAnswer validator cwm (Answer i) = either
  throwAnswerError
  cwm
  (validator i)

readAnswer :: Read b => String -> (b -> Answered a) -> Answer String -> Answered a
readAnswer errMsg cwm (Answer i) = maybe
  (throwAnswerError errMsg)
  cwm
  (readMaybe i)

intAnswer :: (Int -> Answered a) -> Answer String -> Answered a
intAnswer = readAnswer "Please provide a number."

selectAnswer :: String -> [([String], Answered a)] -> Answer String -> Answered a
selectAnswer errMsg acceptables (Answer i) = fromMaybe
  (throwAnswerError errMsg)
  (snd <$> find (elem i . fst) acceptables)

yesNoAnswer :: Answered a -> Answered a -> Answer String -> Answered a
yesNoAnswer yesAns noAns ans = selectAnswer
  "Please answer with either yes or no."
  [(["y", "yes"], yesAns), (["n", "no"], noAns)]
  (map toLower <$> ans)


(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2
infixr 9 <.>
