-- Modul zur Verarbeitung der CLI-Befehle
module CLI (handleCommand) where

import DataHandler (loadRecords, saveRecords)
import Record (Record(..))

-- Diese Funktion empfängt den Befehl und seine Argumente
handleCommand :: String -> [String] -> IO ()
handleCommand cmd args = case cmd of

    -- ===========================
    -- --insert <file> <id> <name> <value>
    -- ===========================
    "--insert" ->
        case args of
            (file:idStr:name:valueStr:_) ->
                case (reads idStr, reads valueStr) of
                    ([(rid,"")], [(val,"")]) -> do
                        records <- loadRecords file
                        let newRecord = Record rid name val
                        let updated = records ++ [newRecord]
                        saveRecords file updated
                        putStrLn "✔ Record inserted."
                    _ -> putStrLn "Fehler: ID und Value müssen Zahlen sein."

            _ -> putStrLn "Usage: --insert <file> <id> <name> <value>"

    -- ===========================
    -- --delete <file> <id>
    -- ===========================
    "--delete" ->
        case args of
            (file:idStr:_) ->
                case reads idStr of
                    [(rid,"")] -> do
                        records <- loadRecords file
                        let updated = filter (\r -> Record.id r /= rid) records
                        saveRecords file updated
                        putStrLn "✔ Record deleted (falls vorhanden)."
                    _ -> putStrLn "Fehler: ID muss eine Zahl sein."

            _ -> putStrLn "Usage: --delete <file> <id>"

    -- Fallback für unbekannte Befehle
    _ -> do
        putStrLn $ "Empfangener Befehl: " ++ cmd
        putStrLn $ "Argumente: " ++ show args
        putStrLn "Unbekannter Befehl."
