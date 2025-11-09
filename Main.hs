-- Hauptmodul des Programms (Einstiegspunkt)
module Main where

import qualified CLI as CLI

main :: IO ()
main = do
  putStrLn "Bitte gib den vollständigen Befehl ein (z.B. --insert data.json Ali 100):"
  input <- getLine
  let args = words input
  case args of
    (cmd:rest) -> CLI.handleCommand cmd rest
    _ -> putStrLn "Usage: <command> [options]"
