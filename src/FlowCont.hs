{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, FunctionalDependencies #-}

module FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..),
  Cont(..), cont, start, end, readAnswer, intAnswer, selectAnswer, yesNoAnswer, validateAnswer_,
  -- (>-*),
  State(..), IsState(..),
  AnswerError(..), throwAnswerError, Answered, runAnswered, ContWithMessage(..), withMessage) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Data.Char (toLower)

import Control.Applicative.Utils ((<.>))

-- | Answer from user
newtype Answer a = Answer { unAnswer :: a } deriving Functor

-- | Each step in the flow can continue with either a
-- @Cont newState@: to update the state of the flow continue the flow to a new step
-- @Start currentState initState@: to start a new (sub-) flow
-- @End finalState@: to end the current flow (or sub flow).
data Cont =
    Question (String, Answer String -> Answered State)
  | Fork (State, String -> Answered State)
  | End State

-- | Each step of the flow returns an optional message along with continuation instruction
data ContWithMessage = ContWithMessage {
  contWith :: Cont,
  contMessage :: Maybe String
}

withoutMessage :: Cont -> ContWithMessage
withoutMessage c = ContWithMessage c Nothing

withMessage :: ContWithMessage -> String -> ContWithMessage
ContWithMessage c _ `withMessage` s = ContWithMessage c (Just s)

-- | Answer processing error
newtype AnswerError = AnswerError String

-- Answer -> IO (Either AnswerError c)
newtype Answered c = Answered {
    unAnswered :: ExceptT AnswerError IO c
  } deriving (Functor, Applicative, Monad, MonadError AnswerError, MonadIO)

runAnswered :: Answered c -> IO (Either AnswerError c)
runAnswered = runExceptT . unAnswered

throwAnswerError :: String -> Answered a
throwAnswerError = throwError . AnswerError

-- | Continue in the same flow
cont :: IsState s => String -> (Answer String -> Answered s) -> ContWithMessage
cont q fs = withoutMessage $ Question (q, state <.> fs)

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> (s -> Answered s') -> ContWithMessage
start s f = withoutMessage $ Fork (state s, state <.> f . read)

-- | Save the latest result @ms@ in the Stack and fork a new flow.
-- (>-*) :: (IsState s, IsState s') => Answered s -> s' -> Answered ContWithMessage
-- ms >-* s' = flip start s'<$> ms
-- infixr 5 >-*

-- | End the current flow
end :: IsState s => s -> ContWithMessage
end = withoutMessage . End . state

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: ContWithMessage,
  save :: String
}

-- | Stack is list of states
type Stack = [State]

serialize :: Stack -> [String]
serialize = map save

-- | Here we have to provide some type in order to know, which
-- read functions to use
deserialize :: IsState s => [String] -> Maybe [s]
deserialize = mapM readMaybe

-- | Convert actual data to stack
stack :: IsState s => [s] -> Stack
stack = map state

-- | A main flow. First type param @s@ is the suspended type of the flow and the second
-- type param @s'@ is the union of suspended types of every sub-flow that this flow depends on (using 'BiState')
class (IsState s, IsState s') => IsFlow s s' | s -> s' where
  deseralizeFlow :: s -> [String] -> Maybe [s']

class (Read s, Show s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> ContWithMessage

  state :: s -> State
  state x = State {
      next = step x,
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

validateAnswer_ :: (String -> IO (Either String b)) -> (b -> Answered a) -> Answer String -> Answered a
validateAnswer_ validator cwm (Answer i) = liftIO (validator i) >>= either
  throwAnswerError
  cwm

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
