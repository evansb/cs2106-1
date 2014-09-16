
module PBuddy.Process where

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V

import Prelude hiding (foldr)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldr)
import Data.Traversable (traverse)

import Control.Applicative
import Control.Monad.State

import PBuddy.Types

-- Create a new PCB
createPCB :: PID -> PID -> Priority -> PCB
createPCB pid0 parent0 priority0 = PCB {
    pid         = pid0
  , priority    = priority0
  , resources   = V.fromList [0,0,0,0,0]
  , status      = Ready
  , parent      = parent0
  , children    = S.empty
}

-- PCB definition for the init process.
initProcess :: PCB
initProcess = createPCB '&' '&' Init

-- PCB definition for the null process.
nilProcess :: PCB
nilProcess = createPCB '^' '^' Init

-- Initial state of the process table.
initProcTable :: M.Map PID PCB
initProcTable = M.fromList [(pid initProcess, initProcess)]

-- Register PCB to the process table
-- If PID already exists, throws an error.
registerPCB :: PCB -> REPL ()
registerPCB pcb0 =
        modify (\os -> os {
            procTable = M.insert (pid pcb0) pcb0 (procTable os)
        })

-- Applies function f to a PCB inside the process table and store it
-- back. If pid does not exist, throws an error.
updatePCB :: (PCB -> PCB) -> PID -> REPL ()
updatePCB f pid0 = do
        pcb0 <- getPCB pid0
        let pcb' = const $ Just (f pcb0)
        modify (\os -> os {
            procTable = M.update pcb' pid0 (procTable os)
        })

-- Remove a PCB from the process table
unregisterPCB :: PCB -> REPL ()
unregisterPCB pcb0 =
        modify (\os -> os {
            procTable = M.delete (pid pcb0) (procTable os)
        })

-- Fetch a PCB from the process table.
-- Return nilProcess if PID does not exist.
getPCB :: PID -> REPL PCB
getPCB pid0 = (fromMaybe nilProcess . M.lookup pid0 . procTable) <$> get

-- Retrieve the PCB of currently running process
getRunningPCB :: REPL PCB
getRunningPCB = get >>= getPCB . running

-- Return process with highest priority.
highestPriority :: REPL PID
highestPriority = do
        os <- get
        priorities <- traverse getPCB (ready os)
        let highest = foldr max initProcess priorities
        return $ pid highest

-- Set the status of a process
setStatus :: Status -> PID -> REPL ()
setStatus s = updatePCB (\p -> p { status = s })
