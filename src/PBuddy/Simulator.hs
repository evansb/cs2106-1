
module PBuddy.Simulator where

import Text.Parsec
import Control.Monad.State
import Control.Monad.Trans.Writer
import PBuddy.Types
import PBuddy.Manager
import PBuddy.Parser
import Data.List (intercalate)
import qualified Data.List.Split as S

-- | Execute REPL actions sequentially.
execCommands :: [REPL PID] -> (String, String)
execCommands cmds =
        let (ans, res) = evalState (runWriterT (sequence cmds)) initialOS
        in (formatOutput ans, res)

-- | Run the simulator on a string and output the result.
runSimulator :: String -> IO ()
runSimulator content = do
        let commandList = case parse commands "" content of
                            Left pe -> error $ show pe
                            Right res -> res
        putStrLn (fst (execCommands (return initPID : commandList)))

-- | Run the simulator on a string and output the result and the log.
runSimulatorVerbose :: String -> IO ()
runSimulatorVerbose content = do
        let commandList = case parse commands "" content of
                            Left pe -> error $ show pe
                            Right res -> res
        let (ans, res) = execCommands (return initPID : commandList)
        putStr res
        putStr ans

-- Miscellaneous functions
render :: PID -> String
render x
    | x == initPID    = "init"
    | x == resetPID   = "init"
    | x == errorPID   = "error"
    | otherwise       = [x]

formatOutput :: String -> String
formatOutput = intercalate "\n\n"
             . map (unwords . map render)
             . S.split (S.keepDelimsL $ S.oneOf [resetPID])
