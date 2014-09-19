
module PBuddy.Types where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

type RID      = Int -- ^ Resource ID
type REPL a   = WriterT String (State OS) a -- ^ REPL Action
type PID      = Char -- ^ Process ID

data RCB = RCB {
      rid :: RID -- ^ Resource ID.
    , unit :: Int -- ^ # of units.
    , blocked :: S.Seq (PID, Int) -- ^ Blocked list.
} deriving (Eq, Show)

data OS = OS {
      procTable    :: M.Map PID PCB -- ^ PID-PCB Table.
    , allResources :: V.Vector RCB -- ^ Status of all resources.
    , running      :: PID -- ^ Current running process PID.
    , ready        :: S.Seq PID -- ^ Ready list.
} deriving (Eq, Show)

data Priority = Init -- ^ Only init process can have this priority.
              | User -- ^ User process.
              | System -- ^ System process.
              deriving (Eq, Show, Ord)

data PCB = PCB {
      pid       :: PID  -- ^ Process Id.
    , priority  :: Priority -- ^ Priority of the process.
    , resources :: V.Vector Int -- ^ Resources attained.
    , status    :: Status -- ^ Which list this process in .
    , parent    :: PID -- ^ PID of the parent.
    , children  :: S.Seq PID -- ^ List of children.
} deriving (Show, Eq)

data Status = Ready
            | Blocked
            | Running
            deriving (Eq, Show)

instance Ord PCB where
    pcb0 <= pcb1 = priority pcb0 < priority pcb1
