module FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsState(..), IsQuestion(..)) where

newtype Answer = Answer { unAnswer :: String }

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

-- | Continue in the same flow
cont :: IsState s => s -> IO Cont
cont i = Cont <$> state i

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> s' -> IO Cont
start s s' = Start <$> state s <*> state s'

-- | End the current flow
end :: IsState s => s -> IO Cont
end s = End <$> state s

-- | Whether the state is a Question
class IsQuestion s where
  ask :: s -> Maybe String

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Answer -> IO Cont,
  question :: Maybe String,
  save :: String
}


class (Read s, Show s, IsQuestion s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> Answer -> IO Cont

  state :: s -> IO State
  state x = return State {
    next = step x,
    question = ask x,
    save = show x
  }

instance Show State where
  show = save
