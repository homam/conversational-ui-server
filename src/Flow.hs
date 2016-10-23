{-# LANGUAGE TypeOperators #-}

module Flow (StepResult(..), receiveAnswer, main) where

import qualified Size
import qualified TryAtHome
import qualified Checkout
import FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsQuestion(ask), IsState(step, state), AnswerError(..), Answered, runAnswered, ContWithMessage(..))

import Control.Arrow (first)
import Text.Read (Read(readsPrec), readMaybe)
import Data.Maybe (fromMaybe, isJust)
import qualified System.IO as IO

-- | Stack is list of states
type Stack = [State]

run :: Stack -> Answer String -> Answered ([Maybe String], Stack)
run [] _ = return ([], [state $ TryAtHome.Suspended TryAtHome.AskProduct ()]) -- initial state
run (s : ss) i = first reverse <$> (proceed [] ss =<< next s i)

proceed :: [Maybe String] -> Stack -> ContWithMessage -> Answered ([Maybe String], Stack)
proceed fs rest (ContWithMessage (Cont s) msg)     = return (msg : fs, s : rest)
proceed fs rest (ContWithMessage (Start s s') msg) = return (msg : fs, s' : s : rest)
proceed fs rest (ContWithMessage (End s) msg)      = case rest of
  (h:t) -> next h (Answer $ show s) >>= proceed (msg : fs) t
  []    -> return (msg : fs, [])


serialize :: Stack -> [String]
serialize = map save

-- | Here we have to provide some type in order to know, which
-- read functions to use
deserialize :: IsState s => [String] -> Maybe [s]
deserialize = mapM readMaybe


-- | Convert actual data to stack
stack :: IsState s => [s] -> Stack
stack = map state

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


loopStack :: Stack -> IO ()
loopStack [] = putStrLn "Fin"
loopStack current@(h:_) = do
  maybe (return ()) (putStrLn . (">> " ++)) (question h)
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case (deserialize saved :: Maybe [Checkout.Suspended :|: Size.Suspended :|: TryAtHome.Suspended]) of
    Just loaded -> do
      let beforeStack = stack loaded
      i <- Answer <$> readLn
      state <- runAnswered $ run beforeStack i
      case state of
        Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack beforeStack
        Right (msgs, stk) -> mapM_ (maybe (return ()) putStrLn) msgs >> loopStack stk
    Nothing -> putStrLn "parse error"


data StepResult = StepResult {
  stepQuestion :: Maybe String,
  stepSerializedState :: String,
  stepBadAnswerError :: Maybe String,
  stepMessage :: [String]
} deriving (Show)

receiveAnswer :: String -> String -> IO StepResult
receiveAnswer saved i =
  case (deserialize (read saved) :: Maybe [Checkout.Suspended :|: Size.Suspended :|: TryAtHome.Suspended]) of
    Just loaded -> do
      let beforeStack = stack loaded
      state <- runAnswered $ run beforeStack (Answer i)
      return $ case state of
        Left (AnswerError e) ->
          let (q, s) = go beforeStack
          in StepResult {
              stepQuestion = q,
              stepSerializedState = s,
              stepBadAnswerError = Just e,
              stepMessage = []
            }
        Right (msg, stk) ->
          let (q, s) = go stk
          in StepResult {
              stepQuestion = q,
              stepSerializedState = s,
              stepBadAnswerError = Nothing,
              stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] msg
            }
    Nothing -> error $ "Cannot parse: " ++ saved
    where
      go :: Stack -> (Maybe String, String)
      go stk =
        let serialized = show $ serialize stk in
        case stk of
          []    -> (Nothing, serialized)
          (h:_) -> (question h, serialized)


main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  let start = [state $ TryAtHome.Suspended TryAtHome.AskProduct ()]
  loopStack start
