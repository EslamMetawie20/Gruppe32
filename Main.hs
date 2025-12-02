-- Hauptmodul des Programms
module Main where

import System.Environment (getArgs)
import CLI
import Shell (shell)

-- Liest die Argumente und leitet sie an das CLI-Modul weiter
-- Wenn keine Argumente: Startet interaktive Shell
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> shell
        (cmd:rest) -> CLI.handleCommand cmd rest
