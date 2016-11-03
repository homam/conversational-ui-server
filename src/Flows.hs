module Flows (FlowId(..), StepResult(..), receiveAnswer, consoleApp) where

import Flow (StepResult(..), processAnswer, runFlowInConsole)
-- import qualified Flows.Size as Size
-- import qualified Flows.TryAtHome as TryAtHome
-- import qualified Flows.Checkout as Checkout
import qualified Flows.Airport as Airport
import qualified Flows.BookATicket as BookATicket

data FlowId = TryAtHome | Size | Airport | BookATicket deriving (Show, Read)

receiveAnswer :: FlowId -> String -> String -> IO StepResult
-- receiveAnswer TryAtHome = processAnswer (TryAtHome.Suspended TryAtHome.AskProduct ())
-- receiveAnswer Size = processAnswer (Size.Suspended Size.AskDoYou ())
receiveAnswer Airport = processAnswer (Airport.Suspended Airport.AskCity Airport.Origin)
receiveAnswer BookATicket = processAnswer (BookATicket.Suspended BookATicket.AskOrigin ())

consoleApp :: FlowId -> IO ()
-- consoleApp TryAtHome = runFlowInConsole (TryAtHome.Suspended TryAtHome.AskProduct ())
-- consoleApp Size = runFlowInConsole (Size.Suspended Size.AskDoYou ())
consoleApp Airport = runFlowInConsole (Airport.Suspended Airport.AskCity Airport.Origin)
consoleApp BookATicket = runFlowInConsole (BookATicket.Suspended BookATicket.AskOrigin ())

-- for testing in GHCi
main = consoleApp BookATicket

test =
  receiveAnswer BookATicket "[]" ""
  >>= next "Dubai"
  >>= next "DXB"
  >>= next "yes"
  >>= next "Amsterdam"
  >>= next "yes"
  >>= next "--"
  >>= next "2"
  >>= info
  where
    next :: String -> StepResult -> IO StepResult
    next ans res = do
      info res
      putStrLn $ ">> " ++ ans
      receiveAnswer BookATicket (stepSerializedState res) ans
    info res = do
      putStrLn $ replicate 30 '-'
      mapM_ (putStrLn . ("!! " ++)) (stepBadAnswerError res)
      mapM_ (putStrLn . (":: " ++)) (stepMessage res)
      mapM_ (putStrLn . ("?? " ++)) (stepQuestion res)
