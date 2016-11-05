{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, FunctionalDependencies #-}

module FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..),
  Cont(..), cont, start, end, tell, readAnswer, intAnswer, selectAnswer, yesNoAnswer, validateAnswer_, fetchAnswers,
  State(..), IsState(..),
  AnswerError(..), throwAnswerError, Answered, runAnswered) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import qualified Control.Monad.Writer.Lazy as W -- (MonadWriter(..), tell)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Arrow (first)

import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
-- import Data.Char (toLower)

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
  | End (Answered State)

-- | Answer processing error
newtype AnswerError = AnswerError String

-- Answer -> IO (Either AnswerError c)
newtype Answered c = Answered {
    unAnswered :: ExceptT AnswerError (WriterT [String] IO) c
  } deriving (Functor, Applicative, Monad, MonadError AnswerError, W.MonadWriter [String], MonadIO)

runAnswered :: Answered a -> IO (Either AnswerError a, [String])
runAnswered = runWriterT . runExceptT . unAnswered

throwAnswerError :: String -> Answered a
throwAnswerError = throwError . AnswerError

-- | Add a message to the response
tell :: String -> Answered ()
tell = W.tell . return

-- | Continue in the same flow
cont :: IsState s => String -> (Answer String -> Answered s) -> Cont
cont q fs = Question (q, state <.> fs)

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> (s -> Answered s') -> Cont
start s f = Fork (state s, state <.> f . read)

-- | End the current flow
end :: IsState s => Answered s -> Cont
end s = End (state <$> s)

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Cont,
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
  step :: s -> Cont

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

selectAnswerLower :: String -> [([String], Answered a)] -> Answer String -> Answered a
selectAnswerLower errMsg acceptables ans = selectAnswer
  errMsg
  (map (first $ map toLower) acceptables)
  (toLower <$> ans)

yesNoAnswer :: Answered a -> Answered a -> Answer String -> Answered a
yesNoAnswer yesAns noAns ans = selectAnswer
  "Please answer with either yes or no."
  [(["y", "yes"], yesAns), (["n", "no"], noAns)]
  (toLower <$> ans)

fetchAnswers :: String -> (String -> IO [b]) -> ([b] -> Answered a) -> (b -> Answered a) -> Answer String -> Answered a
fetchAnswers errMsg f cs c (Answer i) = next =<< liftIO (f i) where
  next ls@(_:_:_) = cs ls
  next   [h]      = c h
  next    _       = throwAnswerError errMsg

toLower = T.unpack . T.toLower . T.strip . T.pack
