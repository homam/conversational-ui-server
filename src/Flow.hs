{-# LANGUAGE TupleSections #-}

module Flow (StepResult(..), FlowId(..), receiveAnswer, main) where

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

-- | Main function that runs a Stack wih the given answer and returns a new Stack
-- with any optional messages that might have been produced.
run :: Stack -> Answer String -> Answered ([Maybe String], Stack)
run [] _ = return ([], [])
run (s : ss) i = first reverse <$> (proceed [] ss =<< next s i) where

  proceed :: [Maybe String] -> Stack -> ContWithMessage -> Answered ([Maybe String], Stack)
  proceed msgs rest (ContWithMessage (Cont s) msg)     = return (msg : msgs, s : rest)
  proceed msgs rest (ContWithMessage (Start s s') msg) = return (msg : msgs, s' : s : rest)
  proceed msgs rest (ContWithMessage (End s) msg)      = case rest of
    (h:t) -> next h (Answer $ show s) >>= proceed (msg : msgs) t
    []    -> return (msg : msgs, [])

-- | JSON serializable object that represents the result of running n Stack with an Answer.
data StepResult = StepResult {
    stepQuestion :: Maybe String -- ^ Next question if any
  , stepSerializedState :: String -- ^ Next Stack
  , stepBadAnswerError :: Maybe String -- ^ Answer validation error if any
  , stepMessage :: [String] -- ^ List of optional messages
} deriving (Show)

-- | Process the current state and the input from the user.
-- We either return the current step with an error message if input validation fails
-- or move forward to the next step.
processAnswer :: IsFlow s s' => s -> String -> String -> IO StepResult
processAnswer initState saved i =
  case deseralizeFlow initState (read saved) of
    Just loaded -> do
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
    Nothing -> error $ "Cannot parse: " ++ saved

-- | For testing in GHCi
loopStack :: IsFlow s s' => s -> Stack -> IO ()
loopStack _ [] = putStrLn "Fin"
loopStack initState current@(h:_) = do
  maybe (return ()) (putStrLn . (">> " ++)) (question h)
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case deseralizeFlow initState saved of
    Just loaded -> do
      let beforeStack = stack loaded
      i <- Answer <$> readLn
      state <- runAnswered $ run beforeStack i
      case state of
        Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack initState beforeStack
        Right (msgs, stk) -> mapM_ (maybe (return ()) (putStrLn . (":: " ++))) msgs >> loopStack initState stk
    Nothing -> putStrLn "parse error"


data FlowId = TryAtHome | Size deriving (Show, Read)

receiveAnswer :: FlowId -> String -> String -> IO StepResult
receiveAnswer TryAtHome = processAnswer (TryAtHome.Suspended TryAtHome.AskProduct ())
receiveAnswer Size = processAnswer (Size.Suspended Size.AskDoYou ())

-- for testing in GHCi
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  let start = TryAtHome.Suspended TryAtHome.AskProduct ()
  loopStack start [state start]
