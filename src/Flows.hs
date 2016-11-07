module Flows (FlowId(..), StepResult(..), receiveAnswer, consoleApp) where

import Flow (StepResult(..), processAnswer, runFlowInConsole)
import qualified Flows.Checkout as Checkout
import qualified Flows.Airport as Airport
import qualified Flows.BookATicket as BookATicket

data FlowId = Airport | BookATicket deriving (Show, Read) -- TryAtHome | Size |

receiveAnswer :: FlowId -> String -> String -> IO StepResult
receiveAnswer Airport = processAnswer (Airport.Suspended Airport.AskCity Airport.Origin)
receiveAnswer BookATicket = processAnswer (BookATicket.Suspended BookATicket.AskOrigin ())

consoleApp :: FlowId -> IO ()
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
  >>= next "23 Dec 2015"
  >>= next "23 Dec 2016"
  >>= next "Nov 21 2016"
  >>= next "7 Jan 2017"
  >>= info
    where
      next = next' BookATicket

testAirport =
  receiveAnswer Airport "[]" ""
  >>= next "Dubai"
  >>= next "What"
  >>= next "DXB"
  >>= next "y"
  >>= info
  where
    next = next' Airport

next' :: FlowId -> String -> StepResult -> IO StepResult
next' fid ans res = do
  info res
  putStrLn $ ">> " ++ ans
  receiveAnswer fid (stepSerializedState res) ans
info res = do
  putStrLn $ replicate 30 '-'
  mapM_ (putStrLn . ("!! " ++)) (stepBadAnswerError res)
  mapM_ (putStrLn . (":: " ++)) (stepMessage res)
  mapM_ (putStrLn . ("?? " ++)) (stepQuestion res)
