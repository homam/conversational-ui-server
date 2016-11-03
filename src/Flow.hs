{-# LANGUAGE TupleSections #-}

module Flow (StepResult(..), runFlowInConsole, processAnswer) where

import FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..), Cont(..), State(..), IsState(step, state),
  AnswerError(..), Answered, runAnswered)

import Control.Arrow ((>>>))
import qualified System.IO as IO
import Control.Monad ((>=>))
import Debug.Trace (trace)

data RunResult =
    NextQuestion String Stack (Answer String -> Answered RunResult)
  | EndOfFlow (Answered Stack)
  | ForkAFlow Stack (String -> Answered RunResult)

-- | Main function that runs a Stack wih the given answer and returns a new Stack
-- with any optional messages that might have been produced.
run :: Stack -> RunResult
run [] = EndOfFlow $ return []
run istk@(s : ss) = case next s of
  Question (q, fs) ->  NextQuestion q istk (fmap (run . (:ss)) . fs)
  End ns -> EndOfFlow $ (: ss) <$> ns
  Fork (ns, f) -> ForkAFlow (ns : s : ss) (f >>> fmap (run . (: ss))) -- (f >=> (\ s' -> return $ run (s' : ss)))

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
      case beforeStack of
        (_:_) -> case run beforeStack of
          NextQuestion _ _ f -> runAnswered (f $ Answer i) >>= uncurry handleAns
        _ -> handleAns (Right $ run [state initState]) [] -- empty Stack, initialize with initState

        where
          handleAns :: Either AnswerError RunResult -> [String] -> IO StepResult
          handleAns (Left (AnswerError e)) msgs = return StepResult {
              stepQuestion = question,
              stepSerializedState = saved,
              stepBadAnswerError = Just e,
              stepMessage = msgs
            } where
              question = case run (stack loaded) of
                NextQuestion q _ _ -> Just q
                _ -> Nothing
          handleAns (Right (NextQuestion q stk _)) msgs = return StepResult {
              stepQuestion = Just q,
              stepSerializedState = show $ serialize stk,
              stepBadAnswerError = Nothing,
              stepMessage = msgs
            }
          handleAns (Right (EndOfFlow astk)) imsgs = runAnswered astk >>= \ r -> case r of
            (Left (AnswerError e), msgs) -> return StepResult {
              stepQuestion = Nothing,
              stepSerializedState = [],
              stepBadAnswerError = Just e,
              stepMessage = imsgs ++ msgs
            }
            (Right (h : t), msgs) ->
              case run t of
                (ForkAFlow stk' f) -> runAnswered (f $ show h) >>= \ (a, b) -> handleAns a (imsgs ++ msgs ++ b)
                eof@(EndOfFlow astk) -> runAnswered astk >>= \ (_, b) -> handleAns (Right eof) (imsgs ++ msgs ++ b)
            (Right _, msgs) -> return StepResult {
                stepQuestion = Nothing,
                stepSerializedState = [],
                stepBadAnswerError = Nothing,
                stepMessage = imsgs ++ msgs
              }

          handleAns (Right (ForkAFlow stk@(h:t) f)) msgs = handleAns (Right $ run stk) msgs

    Nothing -> error $ "Cannot parse: " ++ saved

-- | For testing in GHCi
loopStack :: Stack -> IO Stack
loopStack [] = putStrLn "Fin" >> return []
loopStack beforeStack =
  case run beforeStack of
    NextQuestion q stk f -> do
      putStrLn $ ">> " ++ q
      i <- Answer <$> readLn
      runAnswered (f i) >>= uncurry handleAns
    EndOfFlow m -> handleEndOfFlow m
    ForkAFlow stk f -> loopStack stk >>= \ (h:t) -> runAnswered (f $ show h) >>= uncurry handleAns
    where
      handleAns ans msgs = do
        mapM_ (putStrLn . (":: " ++)) msgs
        case ans of
          Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack beforeStack
          Right (NextQuestion q stk f) -> loopStack stk
          Right (EndOfFlow m) -> handleEndOfFlow m
          Right (ForkAFlow stk f) -> loopStack stk >>= \ (h:t) -> runAnswered (f $ show h) >>= uncurry handleAns

      handleEndOfFlow m = runAnswered m >>= \ a -> case a of
        (Left (AnswerError e), msgs) -> do
          mapM_ (putStrLn . (":: " ++)) msgs
          putStrLn ("!! " ++ e) >> loopStack beforeStack
        (Right stk', msgs) -> do
          mapM_ (putStrLn . (":: " ++)) msgs
          return stk'


runFlowInConsole :: IsFlow s s' => s -> IO ()
runFlowInConsole s = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  loopStack [state s]
  return ()
