module Shell where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Encode.Pretty (encodePretty)
import CLI (parseCommand, logicInsert, logicUpdate, logicDelete, logicFilter, logicQuery, logicStats)
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
    if null filename
        then shell -- Retry if empty
        else if filename == "create"
            then do
                putStrLn "Dateiname für neue Datei eingeben (z.B. data.json):"
                putStr "> "
                hFlush stdout
                newFilename <- getLine
                saveRecords newFilename []
                putStrLn $ "Datei '" ++ newFilename ++ "' erstellt."
                shell
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
    let argsList = words input
    
    if null argsList
        then runShell filename records
        else do
            let (cmdStr:args) = argsList
            
            case parseCommand cmdStr args of
                Left err -> do
                    putStrLn err
                    runShell filename records
                
                Right command -> executeShellCommand filename records command

executeShellCommand :: FilePath -> [Record] -> Command -> IO ()
executeShellCommand filename records cmd = case cmd of
    Quit -> putStrLn "Bye!"
    
    Help -> do
        putStrLn "Verfügbare Befehle:"
        putStrLn "  insert <ID> <Name> <Wert>  - Fügt einen Eintrag hinzu"
        putStrLn "  update <ID> <Name> <Wert>  - Aktualisiert einen Eintrag"
        putStrLn "  delete <ID>                - Löscht einen Eintrag"
        putStrLn "  filter <Wert>              - Zeigt Einträge > Wert"
        putStrLn "  query <Name>               - Sucht nach Name"
        putStrLn "  stats                      - Zeigt Statistik"
        putStrLn "  list                       - Zeigt alle Einträge (formatiert)"
        putStrLn "  save [Name]                - Speichert Änderungen (optional unter neuem Namen)"
        putStrLn "  quit                       - Beendet die Shell"
        runShell filename records

    Insert i n v -> do
        case logicInsert i n v records of
            Left err -> do
                putStrLn err
                runShell filename records
            Right updated -> do
                putStrLn "Erfolg: Eintrag hinzugefügt."
                runShell filename updated

    Update i n v -> do
        case logicUpdate i n v records of
            Left err -> do
                putStrLn err
                runShell filename records
            Right updated -> do
                putStrLn "Erfolg: Eintrag aktualisiert."
                runShell filename updated

    Delete i -> do
        case logicDelete i records of
            Left err -> do
                putStrLn err
                runShell filename records
            Right updated -> do
                putStrLn $ "Erfolg: Eintrag mit ID " ++ show i ++ " gelöscht."
                runShell filename updated

    Filter v -> do
        let filtered = logicFilter v records
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
    
    Print -> do -- Alias for List in Shell
        B.putStrLn (encodePretty records)
        runShell filename records

    Save maybeFile -> do
        let targetFile = case maybeFile of
                            Just f  -> f
                            Nothing -> filename
        saveRecords targetFile records
        putStrLn $ "Gespeichert in " ++ targetFile
        runShell filename records

    Version -> do
        putStrLn "Haskell JSON Manager v1.0.0"
        runShell filename records

    Unknown c -> do
        putStrLn $ "Befehl nicht erkannt: " ++ c ++ ". Nutze 'help' für Hilfe."
        runShell filename records
