{-# LANGUAGE DoAndIfThenElse #-}

module PBuddy.Manager where

import qualified Data.Sequence as S
import qualified Data.Vector as V

import Prelude hiding (foldr)
import Data.Traversable (traverse)
import Data.Foldable (foldr)
import Data.List (intercalate)
import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Trans.Writer

import PBuddy.Types
import PBuddy.Process
import PBuddy.Resource

-- Definition of initial OS State
initialOS :: OS
initialOS =
        OS {
             procTable    = initProcTable
           , allResources = initRCBs
           , running      = pid initProcess
           , ready        = S.singleton (pid initProcess)
           }

initPID = '&'
errorPID = '-'
resetPID = '!'

-- | Reset the OS to initial state, return reset PID
reset:: REPL PID
reset = do
        put initialOS
        tell "[SYS] System initialized\n"
        return resetPID

-- | Create a process by specifying a PID and a Priority.
--   Call scheduler in the end.
createProc :: PID -> Priority -> REPL PID
createProc cpid prio = do
        tell ("Creating Process " ++ show (cpid,prio) ++ "\n")
        ppid <- running <$> get
        tryGet <- getPCB cpid
        --  Cannot create if PID already exists
        let pidExists = tryGet /= nilProcess
        --  Cannot create process with init priority
        let invalidPID = prio == Init
        if invalidPID || pidExists then
            return errorPID
        else do
            let childPCB = createPCB cpid ppid prio
            addChild cpid ppid
            addToRL cpid
            registerPCB childPCB
            scheduler

-- | Kill a process and free hold resources.
--   Call scheduler in the end.
killProc :: PID -> REPL PID
killProc pid0 = do
        tell ("Killing Process " ++ [pid0] ++ "\n")
        os <- get
        -- Cannot kill init process.
        let notInit = pid0 /= initPID
        -- Can only kill descendants.
        isAncestor <- running os `isAncestorOf` pid0
        let canKill = notInit && isAncestor
        if canKill then
            killProcInner pid0
        else return errorPID

-- | Kill a process, assuming the process id is valid and can be killed
--   by current running process.
killProcInner :: PID -> REPL PID
killProcInner pid0 = do
        pcbToDelete <- getPCB pid0
        -- Release all resources.
        releaseAll pid0
        -- Kill the whole descendants
        traverse killProcInner (children pcbToDelete)
        -- Unregister it
        unregisterPCB pcbToDelete
        -- Remove from ready lsit if it is there
        removeFromRL pid0
        -- Remove from blocked list if it is there.
        mapM_ (removeFromWaitingList pid0) [1..4]
        tell ("[Success] Killed : " ++ [pid0] ++ "\n")
        scheduler

-- | Request a timeout. Place back process to ready list and inquire new
--   highest priority process.
timeout :: REPL PID
timeout = do
        tell "Timeout\n"
        current <- running <$> get
        removeFromRL current
        setStatus Ready current
        addToRL current
        highest <- highestPriority
        setStatus Running highest
        scheduler

-- | Request some units of resources.
requestResource :: RID -> Int -> REPL PID
requestResource rid0 unit0 = do
        tell ("Requesting " ++ show unit0 ++ " of " ++ show rid0 ++ "\n")
        os <- get
        let rcb0 = allResources os V.! rid0
        -- # of units must suffice
        let enoughUnit = unit rcb0 >= unit0
        -- # of units must be valid
        let invalidUnit = unit0 < 0 || unit0 > rid0
        -- Process must not be init
        let runningIsInit = running os == initPID
        if invalidUnit || runningIsInit then
            return errorPID
        else do
            if not enoughUnit
            then do
                waitResource rid0 unit0
                tell ("[Res] Blocked process " ++ [running os] ++ "\n")
            else do
                alterHoldResource (+ unit0) rid0
                decrResource rid0 unit0
                tell ("[Res] Resource assigned " ++ [running os] ++ "\n")
            scheduler

-- | The inner relesase resource function.
--   Assume all conditions are satisfied beforehand
releaseR :: PID -> RID -> Int -> REPL ()
releaseR pid0 rid0 unit0 = unless (unit0 <= 0) $ do
        pcb0 <- getPCB pid0
        let decr x = x - unit0
        -- Decrement from vector and update
        let resources' = alterVector decr rid0 (resources pcb0)
        updatePCB (\p -> p { resources = resources' }) pid0
        -- Add the resource back
        decrResource rid0 (- unit0)
        feedDepraved rid0
        tell ("[Res] Released " ++ show unit0  ++ " of "
                ++ show rid0 ++ "\n")

-- | Release all resources.
releaseAll :: PID -> REPL ()
releaseAll pid0 = do
        resources0 <- resources <$> getPCB pid0
        mapM_ (\x -> releaseR pid0 x (resources0 V.! x)) [1..4]

-- | Release some units of resources
releaseResource :: RID -> Int -> REPL PID
releaseResource rid0 unit0 = do
        tell ("Releasing " ++ show unit0 ++ " of " ++ show rid0 ++ "\n")
        current <- running <$> get
        pcb0 <- getPCB current
        let unit1 = resources pcb0 V.! rid0
        -- Cannot release more than it holds.
        let invalidUnit = unit0 <= 0 || unit0 > unit1
        if invalidUnit then
            return errorPID
        else do
            releaseR current rid0 unit0
            scheduler

-- | Place the running process to the back of the queue
--   Get new highest priority process
scheduler :: REPL PID
scheduler = do
        os <- get
        runPCB <- getRunningPCB
        highest <- highestPriority >>= getPCB
        let chosen = if (runPCB < highest) || (status runPCB /= Running)
            then pid highest
            else running os
        tell $ if chosen /= running os
            then  "[Scheduler] Switched to " ++ [chosen] ++ "\n\n"
            else  "\n"
        put os {
            running = chosen
        }
        setStatus Running chosen
        return chosen

-------------------------------
--- Miscellaneous Functions  ---


-- Recursively check if a process is an ancestor of another process.
isAncestorOf :: PID -> PID -> REPL Bool
ppid `isAncestorOf` cpid = do
        ppcb <- getPCB ppid
        let samePID = ppid == cpid
        let hasChild = not $ S.null (children ppcb)
        rec <- traverse (`isAncestorOf` cpid) (children ppcb)
        let isAncestor = not $ S.null $ S.filter (== True) rec
        return $ samePID || (hasChild && isAncestor)

-- Decrement some unit to a resource
decrResource :: RID -> Int -> REPL ()
decrResource rid0 d =
        updateRCB (\r -> let r' = r { unit = unit r - d } in r') rid0

-- Print the ready list for debugging purposes.
printReadyList :: REPL ()
printReadyList = do
        rl <- ready <$> get
        let f = (:) . (:[])
        let str = intercalate "," (foldr f [] rl) ++ "\n"
        tell str

-- Add child to a process, updates the PCB.
addChild :: PID -> PID -> REPL ()
addChild cpid =
        updatePCB (\p -> let children' = children p S.|> cpid
                         in p { children = children'})

-- Add process to ready list.
addToRL :: PID -> REPL ()
addToRL ppid = do
        modify (\os -> os {
            ready = ready os S.|> ppid
        })
        tell ("[ReadyList] Added: " ++ [ppid] ++ "\n")
-- Remove process from ready list.
removeFromRL :: PID -> REPL ()
removeFromRL ppid = do
        modify (\os -> os {
            ready = S.filter (/= ppid) (ready os)
        })
        tell ("[ReadyList] Removed " ++ [ppid] ++ "\n")

-- Make a process wait for resource
waitResource :: RID -> Int -> REPL ()
waitResource rid0 unit0 = do
        pid0 <- running <$> get
        setStatus Blocked pid0
        addToWaitingList rid0 pid0 unit0

-- Alter currently hold resource.
alterHoldResource :: (Int -> Int) -> RID -> REPL ()
alterHoldResource f rid0 = do
        pcb0 <- getRunningPCB
        let resources' = alterVector f rid0 (resources pcb0)
        updatePCB (\p -> p { resources = resources' }) (pid pcb0)

-- Add a process to the waiting list
addToWaitingList :: RID -> PID -> Int -> REPL ()
addToWaitingList rid0 pid0 unit0 = do
        rcb0 <- getRCB rid0
        let newRCB = rcb0 {
            blocked = blocked rcb0 S.|> (pid0, unit0)
        }
        updateRCB (const newRCB) rid0
        removeFromRL pid0
        modify (\os -> os { running = '0' })

-- Remove a process from the waiting list
removeFromWaitingList :: PID -> RID -> REPL ()
removeFromWaitingList pid0 rid0 = do
        rcb0 <- getRCB rid0
        unit0 <- ((V.! rid0) . resources) <$> getPCB pid0
        let bloc = blocked rcb0
        let newRCB = rcb0 {
            blocked = removeSequence (pid0, unit0) bloc
        }
        updateRCB (const newRCB) rid0

removeSequence :: (Eq a) => a -> S.Seq a -> S.Seq a
removeSequence el =
        foldr (\e y -> if e == el
                       then  e S.<| y
                       else y) S.empty

-- Feed blocked status with resource
feedDepraved :: RID -> REPL ()
feedDepraved rid0 = do
        rcb0 <- getRCB rid0
        let waitingList  = blocked rcb0
        let unit0 = unit rcb0
        case S.viewl waitingList of
            S.EmptyL -> return ()
            (pid0, unit1) S.:< xs ->
                when (unit0 >= unit1) $ do
                    setStatus Ready pid0
                    addToRL pid0
                    revec <- resources <$> getPCB pid0
                    let resources' = alterVector (+ (unit0 - unit1)) rid0 revec
                    updatePCB (\p -> p { resources = resources' }) pid0
                    updateRCB (\r -> r { blocked = xs }) rid0
                    feedDepraved rid0
