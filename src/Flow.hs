{-# LANGUAGE TupleSections #-}

module Flow (StepResult(..), receiveAnswer, main) where

import qualified Flows.Size as Size
import qualified Flows.TryAtHome as TryAtHome
import qualified Flows.Checkout as Checkout
import FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..), Cont(..), State(..), IsQuestion(ask), IsState(step, state),
  AnswerError(..), Answered, runAnswered, ContWithMessage(..))

import Control.Arrow (first)
import Text.Read (Read(readsPrec), readMaybe)
import Data.Maybe (fromMaybe, isJust)
import qualified System.IO as IO


run :: Stack -> Answer String -> Answered ([Maybe String], Stack)
run [] _ = return ([], []) -- initial state
run (s : ss) i = first reverse <$> (proceed [] ss =<< next s i)

proceed :: [Maybe String] -> Stack -> ContWithMessage -> Answered ([Maybe String], Stack)
proceed fs rest (ContWithMessage (Cont s) msg)     = return (msg : fs, s : rest)
proceed fs rest (ContWithMessage (Start s s') msg) = return (msg : fs, s' : s : rest)
proceed fs rest (ContWithMessage (End s) msg)      = case rest of
  (h:t) -> next h (Answer $ show s) >>= proceed (msg : fs) t
  []    -> return (msg : fs, [])


data StepResult = StepResult {
  stepQuestion :: Maybe String,
  stepSerializedState :: String,
  stepBadAnswerError :: Maybe String,
  stepMessage :: [String]
} deriving (Show)

receiveAnswer = receiveAnswer' (TryAtHome.Suspended TryAtHome.AskProduct ())

-- | Process the current state and the input from the user.
-- We either return the current step with an error message if input validation fails
-- or move forward to the next step.
receiveAnswer' :: IsFlow s s' => s -> String -> String -> IO StepResult
receiveAnswer' sflow saved i =
  case deseralizeFlow sflow (read saved) of
    Just loaded -> processAnswer sflow loaded i
    Nothing -> error $ "Cannot parse: " ++ saved

processAnswer :: (IsState s, IsState s') => s' -> [s] -> String -> IO StepResult
processAnswer initState loaded i = do
  let beforeStack = stack loaded
  state <- runAnswered $ case beforeStack of
    (_:_) -> run beforeStack (Answer i)
    _ -> return ([], [state initState])
  return $ case state of
    -- validatoin failed
    Left (AnswerError e) ->
      let (q, s) = go beforeStack
      in StepResult {
          stepQuestion = q,
          stepSerializedState = s,
          stepBadAnswerError = Just e,
          stepMessage = []
        }
    -- move forward in the graph
    Right (msg, stk) ->
      let (q, s) = go stk
      in StepResult {
          stepQuestion = q,
          stepSerializedState = s,
          stepBadAnswerError = Nothing,
          stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] msg
        }
  where
    go :: Stack -> (Maybe String, String)
    go stk =
      let serialized = show $ serialize stk in
      case stk of
        []    -> (Nothing, serialized)
        (h:_) -> (question h, serialized)

-- | For testing in GHCi
loopStack :: IsFlow s s' => s -> Stack -> IO ()
loopStack _ [] = putStrLn "Fin"
loopStack sflow current@(h:_) = do
  maybe (return ()) (putStrLn . (">> " ++)) (question h)
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case deseralizeFlow sflow saved of
    Just loaded -> do
      let beforeStack = stack loaded
      i <- Answer <$> readLn
      state <- runAnswered $ run beforeStack i
      case state of
        Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack sflow beforeStack
        Right (msgs, stk) -> mapM_ (maybe (return ()) (putStrLn . (":: " ++))) msgs >> loopStack sflow stk
    Nothing -> putStrLn "parse error"


-- for testing in GHCi
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  let start = TryAtHome.Suspended TryAtHome.AskProduct ()
  loopStack start [state start]
