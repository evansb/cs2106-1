
module PBuddy.Resource where

import qualified Data.Sequence as S
import qualified Data.Vector as V

import Control.Monad.State

import PBuddy.Types

-- Initialize the 4 Resources.
initRCBs :: V.Vector RCB
initRCBs = V.fromList
    [
        RCB 0 0 S.empty
    ,   RCB 1 1 S.empty
    ,   RCB 2 2 S.empty
    ,   RCB 3 3 S.empty
    ,   RCB 4 4 S.empty
    ]

-- Fetch an RCB from the resource table.
getRCB :: RID -> REPL RCB
getRCB rid0 = do
        os <- get
        return $ allResources os V.! rid0

-- Similar with updatePCB, specify a function and a resource and updates
-- it.
updateRCB :: (RCB -> RCB) -> Int -> REPL ()
updateRCB f idx = do
        os <- get
        let    v = allResources os
        let newV = alterVector f idx v
        put $ os { allResources = newV }

-- Helper function to easily alter a vector element.
alterVector :: (a -> a)  -> Int -> V.Vector a -> V.Vector a
alterVector f idx v = V.update v (V.fromList [(idx, f (v V.! idx))])
