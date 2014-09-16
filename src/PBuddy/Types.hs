
module PBuddy.Types where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

type RID      = Int
type REPL a   = WriterT String (State OS) a
type PID      = Char

data RCB = RCB {
    rid :: RID,
    unit :: Int,
    blocked :: S.Seq (PID, Int)
} deriving (Eq, Show)

data OS = OS {
    procTable    :: M.Map PID PCB,
    allResources :: V.Vector RCB,
    running      :: PID,
    ready        :: S.Seq PID
} deriving (Eq, Show)

data Priority = Init
              | User
              | System
              deriving (Eq, Show, Ord)

data PCB = PCB {
    pid       :: PID,
    priority  :: Priority,
    resources :: V.Vector Int,
    status    :: Status,
    parent    :: PID,
    children  :: S.Seq PID
} deriving (Show, Eq)

data Status = Ready
            | Blocked
            | Running
            deriving (Eq, Show)

instance Ord PCB where
    pcb0 <= pcb1 = priority pcb0 < priority pcb1
