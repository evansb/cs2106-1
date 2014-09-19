
module PBuddy.Parser where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.String

import Data.Char (ord)

import PBuddy.Types
import PBuddy.Manager

-- | Parser for reset command
resetCmd :: Parser (REPL PID)
resetCmd = string "init" >> return reset

-- | Parser for create process command
createProcCmd :: Parser (REPL PID)
createProcCmd = do
        _ <- string "cr" <* space
        pid0 <- alphaNum <* space
        prio0 <- getPrioChar <$> digit
        return $ createProc pid0 prio0

-- | Parser for timeout command
timeoutCmd :: Parser (REPL PID)
timeoutCmd = string "to" >> return timeout

-- | Parser for kill process command
killProcCmd :: Parser (REPL PID)
killProcCmd = do
        _ <- string "de" <* space
        pid0 <- (try (string "init") >> return initPID) <|> alphaNum
        return $ killProc pid0

-- | Parser for request resource command
requestResourceCmd :: Parser (REPL PID)
requestResourceCmd = do
        _ <- string "req" <* space
        rid0 <- ordify <$> (char 'R' *> digit <* space)
        unit0 <- ordify <$> digit
        return $ requestResource rid0 unit0

-- | Parser for releaser resource command
releaseResourceCmd :: Parser (REPL PID)
releaseResourceCmd = do
        _ <- string "rel" <* space
        rid0 <- ordify <$> (char 'R' *> digit <* space)
        unit0 <- ordify <$> digit
        return $ releaseResource rid0 unit0

eofl = (try (string "\r\n") <|> string "\n") >> return '\n'

-- | Combinations of all commands
commands :: Parser [REPL PID]
commands = many (choice (map try
    [   resetCmd
      , timeoutCmd
      , createProcCmd
      , killProcCmd
      , requestResourceCmd
      , releaseResourceCmd
    ]) <* many1 eofl)

-- Miscellaneous functions.
eol :: Parser Char
eol = char '\n'

ordify :: Char -> Int
ordify p = ord p - ord '0'

getPrioChar :: Char -> Priority
getPrioChar p = case p of
                    '0' -> Init
                    '1' -> User
                    '2' -> System
                    _   -> Init
