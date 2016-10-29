module Flows (FlowId(..), StepResult(..), receiveAnswer, consoleApp) where

import Flow (StepResult(..), processAnswer, runFlowInConsole)
import qualified Flows.Size as Size
import qualified Flows.TryAtHome as TryAtHome
import qualified Flows.Checkout as Checkout

data FlowId = TryAtHome | Size deriving (Show, Read)

receiveAnswer :: FlowId -> String -> String -> IO StepResult
receiveAnswer TryAtHome = processAnswer (TryAtHome.Suspended TryAtHome.AskProduct ())
receiveAnswer Size = processAnswer (Size.Suspended Size.AskDoYou ())

consoleApp :: FlowId -> IO ()
consoleApp TryAtHome = runFlowInConsole (TryAtHome.Suspended TryAtHome.AskProduct ())
consoleApp Size = runFlowInConsole (Size.Suspended Size.AskDoYou ())

-- for testing in GHCi
main = consoleApp TryAtHome
