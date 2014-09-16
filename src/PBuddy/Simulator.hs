
module PBuddy.Simulator where

import Text.Parsec
import Control.Monad.State
import Control.Monad.Trans.Writer
import PBuddy.Types
import PBuddy.Manager
import PBuddy.Parser
import qualified Data.List.Split as S

render :: PID -> String
render x
    | x == initPID    = "init"
    | x == resetPID   = "init"
    | x == errorPID   = "error"
    | otherwise       = [x]

formatOutput :: String -> String
formatOutput = unlines
             . map (unwords . map render)
             . S.split (S.keepDelimsL $ S.oneOf [resetPID])

execCommands :: [REPL PID] -> (String, String)
execCommands cmds =
        let (ans, res) = evalState (runWriterT (sequence cmds)) initialOS
        in (formatOutput ans, res)

runSimulator :: String -> IO ()
runSimulator content = do
        let commandList = case parse commands "" content of
                            Left pe -> error $ show pe
                            Right res -> res
        putStr (fst (execCommands (return initPID : commandList)))

runSimulatorVerbose :: String -> IO ()
runSimulatorVerbose content = do
        let commandList = case parse commands "" content of
                            Left pe -> error $ show pe
                            Right res -> res
        let (ans, res) = execCommands (return initPID : commandList)
        putStr res
        putStr ans
