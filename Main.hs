-- Hauptmodul des Programms (Einstiegspunkt)
module Main where

import System.Environment (getArgs)
import qualified CLI as CLI

-- Liest Kommandozeilenargumente und leitet sie an das CLI-Modul weiter
main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:rest) -> CLI.handleCommand cmd rest
        _          -> putStrLn "Usage: <command> [args]"

