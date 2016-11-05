{-# LANGUAGE TupleSections #-}

module Flow (StepResult(..), runFlowInConsole, processAnswer) where

import FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..), Cont(..), State(..), IsState(step, state),
  AnswerError(..), Answered, runAnswered)

import qualified System.IO as IO
import Debug.Trace (trace)

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
processAnswer initState saved i = case deseralizeFlow initState (read saved) of
  Just loaded -> do
    let  beforeStack = stack loaded
    case beforeStack of
      (_:_) -> processAnswer' [] beforeStack (Just i)
      _     -> processAnswer' [] [state initState] Nothing
  Nothing -> error $ "Cannot parse: " ++ saved

processAnswer' :: [String] -> Stack -> Maybe String -> IO StepResult
processAnswer' imsgs istk@(s:ss) mi = case next s of
  Question (q, fs) -> case mi of
    Nothing -> return StepResult { stepQuestion = Just q, stepSerializedState = show $ serialize istk, stepBadAnswerError = Nothing, stepMessage = imsgs }
    Just i -> runAnswered (fs $ Answer i) >>= uncurry (handleAns $ Just q)
  End ns -> runAnswered ns >>= uncurry handleEOF
  Fork (ns, fs) -> case mi of
    Nothing -> processAnswer' imsgs (ns:istk) Nothing
    Just i -> runAnswered (fs i) >>= uncurry (handleAns Nothing)
  where
    handleAns :: Maybe String -> Either AnswerError State -> [String] -> IO StepResult
    handleAns q (Right s') msgs' = processAnswer' (imsgs ++ msgs') (s':ss) Nothing
    handleAns q (Left (AnswerError e)) msgs' = return StepResult { stepQuestion = q, stepSerializedState = show $ serialize istk, stepBadAnswerError = Just e, stepMessage = imsgs ++ msgs' }

    handleEOF :: Either AnswerError State -> [String] -> IO StepResult
    handleEOF (Right s') msgs' = processAnswer' (imsgs ++ msgs') ss (Just $ show s')
    handleEOF (Left (AnswerError e)) msgs' = return StepResult { stepQuestion = Nothing, stepSerializedState = show $ serialize istk, stepBadAnswerError = Just e, stepMessage = imsgs ++ msgs' }
processAnswer' imsgs [] _ = return StepResult { stepQuestion = Nothing, stepSerializedState = [], stepBadAnswerError = Nothing, stepMessage = imsgs }

-- | For testing in GHCi
loopStack :: State -> IO State
loopStack s = case next s of
  Question (q, fs) -> do
    putStrLn $ ">> " ++ q
    i <- Answer <$> readLn
    runAnswered (fs i) >>= uncurry (handleAns loopStack)
  End ns -> runAnswered ns >>= uncurry (handleAns return)
  Fork (ns, fs) -> do
    s' <- loopStack ns
    runAnswered (fs $ show s') >>= uncurry (handleAns loopStack)

  where
    handleAns :: (State -> IO State) -> Either AnswerError State -> [String] -> IO State
    handleAns contf (Right s') msgs' = do
      mapM_ (putStrLn . (":: " ++)) msgs'
      contf s'
    handleAns _ (Left (AnswerError e)) msgs' = putStrLn ("!! " ++ e) >> loopStack s

runFlowInConsole :: IsFlow s s' => s -> IO ()
runFlowInConsole s = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  loopStack (state s)
  return ()
