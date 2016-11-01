{-# LANGUAGE TupleSections #-}

module Flow (StepResult(..), runFlowInConsole, processAnswer) where

import FlowCont (Stack, serialize, deserialize, stack, IsFlow(..),
  Answer(..), Cont(..), State(..), IsQuestion(ask), IsState(step, state),
  AnswerError(..), Answered, runAnswered, ContWithMessage(..))

import Control.Arrow (first)
import Text.Read (Read(readsPrec), readMaybe)
import Data.Maybe (fromMaybe, isJust)
import qualified System.IO as IO
import Control.Monad ((>=>))
import Debug.Trace (trace)

data RunResult =
    NextQuestion String [Maybe String] Stack (Answer String -> Answered RunResult)
  | EndOfFlow (Answered ([Maybe String], Stack))
  | ForkAFlow [Maybe String] Stack (String -> Answered RunResult)

-- | Main function that runs a Stack wih the given answer and returns a new Stack
-- with any optional messages that might have been produced.
run :: Stack -> RunResult
run [] = EndOfFlow $ return ([], [])
run istk@(s : ss) = case trace (show istk) (next s) of
  ContWithMessage (Question (q, fs)) msg -> NextQuestion q [msg] istk (fmap (run . (:ss)) . fs)
  ContWithMessage (End ns) msg -> EndOfFlow $ return ([msg], ns : ss)
  ContWithMessage (Fork (ns, f)) msg -> ForkAFlow [] (ns : s : ss) (f >=> (\ s' -> return $ run (s' : ss)))

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
      print $ length beforeStack
      case beforeStack of
        (_:_) -> case run beforeStack of
          NextQuestion _ msgs _ f -> runAnswered (f $ Answer i) >>= handleAns msgs -- \ s -> case s of
        _ -> handleAns [] (Right $ run [state initState])

        where
          handleAns :: [Maybe String] -> Either AnswerError RunResult -> IO StepResult
          -- TODO: handle leftAnswerErr
          handleAns imsgs (Right (NextQuestion q msgs stk _)) = return StepResult {
              stepQuestion = Just q,
              stepSerializedState = show $ serialize stk,
              stepBadAnswerError = Nothing,
              stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] (imsgs ++ msgs)
            }
          handleAns imsgs (Right (EndOfFlow mMsgs)) = runAnswered mMsgs >>= \ r -> case r of
            Left (AnswerError e) -> return StepResult {
              stepQuestion = Nothing,
              stepSerializedState = [],
              stepBadAnswerError = Just e,
              stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] imsgs
            }
            Right (msgs, h : t) ->
              case run t of -- TODO: msgs' ++ msgs
                (ForkAFlow msgs' stk' f) -> runAnswered (f $ show h) >>= handleAns (imsgs ++ msgs ++ msgs')
                eof@(EndOfFlow mMsgs) -> handleAns (imsgs ++ msgs) (Right eof)
            Right (msgs, _) -> return StepResult {
                stepQuestion = Nothing,
                stepSerializedState = [],
                stepBadAnswerError = Nothing,
                stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] (imsgs ++ msgs)
              }

          handleAns imsgs (Right (ForkAFlow msgs (h:t) f)) = return StepResult {
              stepQuestion = question h,
              stepSerializedState = show $ serialize (h:t),
              stepBadAnswerError = Nothing,
              stepMessage = foldr (\ m xs -> maybe xs (:xs) m) [] (imsgs ++ msgs)
            }
    Nothing -> error $ "Cannot parse: " ++ saved

-- | For testing in GHCi
loopStack :: Stack -> IO Stack
loopStack [] = putStrLn "Fin" >> return []
loopStack beforeStack =
  case run beforeStack of
    NextQuestion q msgs stk f -> do
      mapM_ (maybe (return ()) (putStrLn . (":: " ++))) msgs
      putStrLn $ ">> " ++ q
      i <- Answer <$> readLn
      runAnswered (f i) >>= handleAns stk
    EndOfFlow m -> handleEndOfFlow m
    ForkAFlow msgs stk f -> loopStack stk >>= \ (h:t) -> trace "** ForkAFlowCallback" $ runAnswered (f $ show h) >>= handleAns t
    where
      handleAns stk ans = case ans of
        Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack beforeStack
        Right (NextQuestion q msgs stk f) -> loopStack stk
        Right (EndOfFlow m) -> handleEndOfFlow m >>= loopStack --TODO: just loopStack
        Right (ForkAFlow msgs stk f) -> loopStack stk >>= \ (h:t) -> trace "** ForkAFlowCallback" $ runAnswered (f $ show h) >>= handleAns t

      handleEndOfFlow m = runAnswered m >>= \ a -> case a of
        Left (AnswerError e) -> putStrLn ("!! " ++ e) >> loopStack beforeStack
        Right (msgs, stk') -> do
          mapM_ (maybe (return ()) (putStrLn . (":: " ++))) msgs
          return stk'


runFlowInConsole :: IsFlow s s' => s -> IO ()
runFlowInConsole s = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  loopStack [state s] -- [state s]
  return ()
