module Flow (main, receiveAnswer) where

import qualified Size
import qualified TryAtHome
import qualified Checkout
import FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsQuestion(ask), IsState(step, state))

import Control.Arrow (first)
import Control.Monad (ap, join)
import Text.Read (Read(readsPrec), readMaybe)
import qualified System.IO as IO

-- | Stack is list of states
type Stack = [State]

run :: Stack -> Answer -> IO Stack
run [] _ = sequence [state $ TryAtHome.Suspended TryAtHome.AskProduct ()] -- initial state
run (s : ss) i = next s i >>= proceed ss

proceed :: Stack -> Cont -> IO Stack
proceed rest (Cont s) = return $ s : rest
proceed rest (Start s s') = return $ s' : s : rest
proceed rest (End s) = case rest of
  (h:t) -> next h (Answer $ show s) >>= proceed t
  []    -> return []

serialize :: Stack -> [String]
serialize = map save

-- | Here we have to provide some type in order to know, which
-- read functions to use
deserialize :: IsState s => [String] -> Maybe [s]
deserialize = mapM readMaybe


-- | Convert actual data to stack
stack :: IsState s => [s] -> IO Stack
stack = mapM state

-- | Union of states, to specify type of 'deserialize'
data BiState l r = LState l | RState r

instance (Read l, Read r, Show l, Show r) => Read (BiState l r) where
  readsPrec p s = map (first LState) (readsPrec p s) ++ map (first RState) (readsPrec p s)

instance (Show l, Show r) => Show (BiState l r) where
  show (LState x) = show x
  show (RState x) = show x

instance (IsQuestion l, IsQuestion r) => IsQuestion (BiState l r) where
  ask (LState x) = ask x
  ask (RState x) = ask x

instance (IsState l, IsState r) => IsState (BiState l r) where
  step (LState x) = step x
  step (RState x) = step x


loopStack :: Stack -> IO ()
loopStack [] = putStrLn "Fin"
loopStack current@(h:_) = do
  maybe (return ()) (putStrLn . (">> " ++)) (question h)
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case (deserialize saved :: Maybe [BiState (BiState Checkout.Suspended Size.Suspended) TryAtHome.Suspended]) of
    Just loaded -> do
      next' <- run <$> stack loaded
      i <- Answer <$> readLn
      ns <- next' i
      loopStack ns
    Nothing -> putStrLn "parse error"


receiveAnswer :: String -> String -> IO (Maybe String, String)
receiveAnswer saved i =
  case (deserialize (read saved) :: Maybe [BiState (BiState Checkout.Suspended Size.Suspended) TryAtHome.Suspended]) of
    Just loaded -> do
      state <- join ((run <$> stack loaded) `ap` return (Answer i))
      let serialized = show $ serialize state
      case state of
        []    -> return (Nothing, serialized)
        (h:_) -> return (question h, serialized)
    Nothing -> error "parse error"

test' :: IO ()
test' = do
  start <- sequence [state $ TryAtHome.Suspended TryAtHome.AskProduct ()]
  loopStack start

main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  test'
