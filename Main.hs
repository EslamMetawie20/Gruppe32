-- Hauptmodul des Programms
module Main where

import System.Environment (getArgs)
import CLI

-- Liest die Argumente und leitet sie an das CLI-Modul weiter
main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:rest) -> CLI.handleCommand cmd rest
        _          -> putStrLn "Usage: <command> [args]"
