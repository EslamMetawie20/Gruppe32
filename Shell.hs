module Shell where

import CLI
import DataHandler
import Types
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- Startet den Shell-Modus
shell :: IO ()
shell = do
    putStrLn "Willkommen im Shell-Modus!"
    putStrLn "Bitte Dateiname eingeben (z.B. data.json):"
    putStr "> "
    hFlush stdout
    filename <- getLine
    if [] == filename
        then shell -- Retry if empty
        else do
            -- Versuche Datei zu laden
            records <- loadRecords filename
            putStrLn $ "Datei '" ++ filename ++ "' geladen. " ++ show (length records) ++ " Einträge."
            runShell filename records

-- Die Hauptschleife der Shell
-- State: Dateiname (zum Speichern) und aktuelle Liste der Records
runShell :: FilePath -> [Record] -> IO ()
runShell filename records = do
    putStr "JSON-Shell> "
    hFlush stdout
    input <- getLine
    let args = words input
    case args of
        [] -> runShell filename records -- Leere Eingabe ignorieren

        ["exit"] -> putStrLn "Bye!"

        ["help"] -> do
            putStrLn "Verfügbare Befehle:"
            putStrLn "  insert <ID> <Name> <Wert>  - Fügt einen Eintrag hinzu"
            putStrLn "  delete <ID>                - Löscht einen Eintrag"
            putStrLn "  filter <Wert>              - Zeigt Einträge > Wert"
            putStrLn "  query <Name>               - Sucht nach Name"
            putStrLn "  stats                      - Zeigt Statistik"
            putStrLn "  out                        - Zeigt alle Einträge"
            putStrLn "  save                       - Speichert Änderungen in die Datei"
            putStrLn "  exit                       - Beendet die Shell"
            runShell filename records

        ("insert":idStr:name:valStr:_) -> do
            case (readMaybe idStr, readMaybe valStr) of
                (Just newId, Just newVal) -> do
                    case logicInsert newId name newVal records of
                        Left err -> do
                            putStrLn err
                            runShell filename records
                        Right updated -> do
                            putStrLn "Eintrag hinzugefügt (im Speicher)."
                            runShell filename updated
                _ -> do
                    putStrLn "Fehler: ID muss Int und Wert muss Double sein."
                    runShell filename records

        ("delete":idStr:_) -> do
            case readMaybe idStr of
                Just rid -> do
                    let updated = logicDelete rid records
                    putStrLn "Eintrag gelöscht (falls vorhanden, im Speicher)."
                    runShell filename updated
                Nothing -> do
                    putStrLn "Fehler: ID muss Int sein."
                    runShell filename records

        ("filter":valStr:_) -> do
            case readMaybe valStr of
                Just threshold -> do
                    let filtered = logicFilter threshold records
                    mapM_ print filtered
                Nothing -> putStrLn "Fehler: Wert muss Zahl sein."
            runShell filename records

        ("query":searchStr:_) -> do
            let found = logicQuery searchStr records
            mapM_ print found
            runShell filename records

        ["stats"] -> do
            putStrLn (logicStats records)
            runShell filename records

        ["out"] -> do
            mapM_ print records
            runShell filename records

        ["save"] -> do
            saveRecords filename records
            putStrLn $ "Gespeichert in " ++ filename
            runShell filename records

        (cmd:_) -> do
            putStrLn $ "Unbekannter Befehl: " ++ cmd
            runShell filename records
