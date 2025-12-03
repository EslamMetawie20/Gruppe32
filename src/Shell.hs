-- | Shell.hs - Interaktiver Shell-Modus (REPL)
--
-- Die Shell läuft in einer Endlosschleife und wartet auf Benutzereingaben.
--
-- Besonderheit: Die Shell hält den Zustand (Records) im Speicher!
-- Änderungen werden erst bei 'save' in die Datei geschrieben.
module Shell where

import qualified Data.ByteString.Lazy.Char8 as B  -- Für JSON-Ausgabe
import Data.Aeson.Encode.Pretty (encodePretty)    -- Schöne JSON-Formatierung
import CLI (parseCommand, logicInsert, logicUpdate, logicDelete, logicFilter, logicQuery, logicStats)
import DataHandler
import Types
import System.IO (hFlush, stdout)  -- Für sofortige Ausgabe (flush)
import Text.Read (readMaybe)

-- | Startet den Shell-Modus
shell :: IO ()
shell = do
    putStrLn $ blue ++ bold ++ "Willkommen im Shell-Modus!" ++ reset
    putStrLn $ blue ++ "Bitte Dateiname eingeben (z.B. data.json):" ++ reset
    putStrLn $ blue ++ "Oder 'create' zum Erstellen einer neuen Datei." ++ reset
    putStr "> "
    hFlush stdout  -- Stellt sicher, dass "> " sofort angezeigt wird
    filename <- getLine
    if null filename
        then shell  -- Leere Eingabe -> nochmal fragen
        else if filename == "create"
            then do
                -- Neue leere Datei erstellen
                putStrLn $ blue ++ "Dateiname für neue Datei eingeben (z.B. data.json):" ++ reset
                putStr "> "
                hFlush stdout
                newFilename <- getLine
                saveRecords newFilename []  -- Leere Liste speichern
                putStrLn $ green ++ "Datei '" ++ newFilename ++ "' erstellt." ++ reset
                shell
        else do
            -- Existierende Datei laden
            records <- loadRecords filename
            putStrLn $ green ++ "Datei '" ++ filename ++ "' geladen. " ++ show (length records) ++ " Einträge." ++ reset
            runShell filename records  -- Hauptschleife starten

-- | Hauptschleife der Shell (REPL)
--
-- Parameter:
--   filename: Dateiname zum Speichern
--   records:  Aktuelle Liste der Records (im Speicher!)
--
-- Die Funktion ruft sich selbst rekursiv auf (Endlosschleife).
-- Bei 'quit' wird die Rekursion beendet.
--
-- Wichtig: 'records' wird bei Änderungen als 'updated' weitergegeben!
runShell :: FilePath -> [Record] -> IO ()
runShell filename records = do
    putStr "JSON-Shell> "
    hFlush stdout
    input <- getLine
    let argsList = words input  -- "insert 1 Max 100" -> ["insert", "1", "Max", "100"]
    
    if null argsList
        then runShell filename records  -- Leere Eingabe ignorieren
        else do
            let (cmdStr:args) = argsList  -- Erstes Wort = Befehl, Rest = Argumente
            
            -- parseCommand gibt Either String Command zurück
            -- Left = Fehler, Right = erfolgreich geparster Befehl
            case parseCommand cmdStr args of
                Left err -> do
                    putStrLn $ red ++ err ++ reset
                    runShell filename records  -- Gleicher State, nochmal
                
                Right command -> executeShellCommand filename records command

-- | Führt einen geparsten Befehl aus
--
-- Pattern Matching auf alle möglichen Commands.
-- Bei erfolgreichen Änderungen wird 'updated' (neue Liste) weitergegeben.
-- Bei Fehlern bleibt 'records' (alte Liste) erhalten.
executeShellCommand :: FilePath -> [Record] -> Command -> IO ()
executeShellCommand filename records cmd = case cmd of
    Quit -> putStrLn $ blue ++ "Bye!" ++ reset
    
    Help -> do
        putStrLn $ blue ++ bold ++ "Verfügbare Befehle:" ++ reset
        putStrLn $ green ++ "  insert <ID> <Name> <Wert>" ++ reset ++ "  - Fügt einen Eintrag hinzu"
        putStrLn $ green ++ "  update <ID> <Name> <Wert>" ++ reset ++ "  - Aktualisiert einen Eintrag"
        putStrLn $ green ++ "  delete <ID>" ++ reset ++ "                - Löscht einen Eintrag"
        putStrLn $ green ++ "  filter <Wert>" ++ reset ++ "              - Zeigt Einträge > Wert"
        putStrLn $ green ++ "  query <Name>" ++ reset ++ "               - Sucht nach Name"
        putStrLn $ green ++ "  stats" ++ reset ++ "                      - Zeigt Statistik"
        putStrLn $ green ++ "  list" ++ reset ++ "                       - Zeigt alle Einträge (formatiert)"
        putStrLn $ green ++ "  save [Name]" ++ reset ++ "                - Speichert Änderungen (optional unter neuem Namen)"
        putStrLn $ green ++ "  quit" ++ reset ++ "                       - Beendet die Shell"
        runShell filename records

    Insert recordId name value -> do
        case logicInsert recordId name value records of
            Left err -> do
                putStrLn $ red ++ err ++ reset
                runShell filename records
            Right updated -> do
                putStrLn $ green ++ "Erfolg: Eintrag hinzugefügt:" ++ reset
                -- Zeige den neu erstellten Record direkt an
                B.putStrLn (encodePretty (Record recordId name value))
                runShell filename updated

    Update recordId name value -> do
        case logicUpdate recordId name value records of
            Left err -> do
                putStrLn $ red ++ err ++ reset
                runShell filename records
            Right updated -> do
                putStrLn $ green ++ "Erfolg: Eintrag aktualisiert." ++ reset
                runShell filename updated

    Delete recordId -> do
        case logicDelete recordId records of
            Left err -> do
                putStrLn $ red ++ err ++ reset
                runShell filename records
            Right updated -> do
                putStrLn $ green ++ "Erfolg: Eintrag mit ID " ++ show recordId ++ " gelöscht." ++ reset
                runShell filename updated

    Filter minValue -> do
        let filtered = logicFilter minValue records
        B.putStrLn (encodePretty filtered)
        runShell filename records

    Query str -> do
        let found = logicQuery str records
        B.putStrLn (encodePretty found)
        runShell filename records

    Stats -> do
        putStrLn (logicStats records)
        runShell filename records

    List -> do
        B.putStrLn (encodePretty records)
        runShell filename records

    Save maybeFile -> do
        let targetFile = case maybeFile of
                            Just file  -> file
                            Nothing -> filename
        saveRecords targetFile records
        putStrLn $ green ++ "Gespeichert in " ++ targetFile ++ reset
        runShell filename records

    Version -> do
        putStrLn $ blue ++ "Haskell JSON Manager v1.0.0" ++ reset
        runShell filename records

    Unknown unknownCmd -> do
        putStrLn $ red ++ "Befehl nicht erkannt: " ++ unknownCmd ++ ". Nutze 'help' für Hilfe." ++ reset
        runShell filename records
