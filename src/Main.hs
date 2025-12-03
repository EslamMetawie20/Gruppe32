-- | Main.hs - Einstiegspunkt des Programms
-- 
-- Das Programm hat zwei Modi:
-- 1. CLI-Modus: Einzelne Befehle mit Argumenten (z.B. --insert data.json 1 Max 100)
-- 2. Shell-Modus: Interaktive Eingabeaufforderung (wenn keine Argumente)
--
-- Aufruf-Beispiele:
--   cabal run grp32-exe                     -> Startet Shell-Modus
--   cabal run grp32-exe -- --help           -> Zeigt Hilfe
--   cabal run grp32-exe -- --insert ...     -> Führt Insert aus
module Main where

import System.Environment (getArgs)  -- Zum Lesen der Kommandozeilen-Argumente
import CLI                           -- CLI-Befehle
import Shell (shell)                 -- Interaktive Shell

-- | Hauptfunktion - wird beim Programmstart aufgerufen
-- getArgs holt die Kommandozeilen-Argumente als Liste von Strings
main :: IO ()
main = do
    args <- getArgs                               -- args :: [String]
    case args of
        []         -> shell                       -- Keine Argumente -> Shell starten
        (cmd:rest) -> CLI.handleCommand cmd rest  -- Sonst CLI-Befehl ausführen
