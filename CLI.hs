-- Modul für die Verarbeitung der CLI-Befehle
module CLI
  ( handleCommand,
    insert,
    delete,
    filterR,
    query,
    stats,
    out,
    help,
    version
  )
where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

import DataHandler
import Record

-- Entscheidet, welcher Befehl ausgeführt wird
handleCommand :: String -> [String] -> IO ()
handleCommand "--insert" args = handleInsert args
handleCommand "--delete" args = handleDelete args
handleCommand "--filter" args = handleFilter args
handleCommand "--query"  args = handleQuery args
handleCommand "--stats"  args = handleStats args
handleCommand "--out"    args = handleOut args
handleCommand "--help"   args = handleHelp args
handleCommand "--version" args = handleVersion args


-- handleCommand "--update" args = handleUpdate args



-- Interface bauen ?!?
-- Ein mögliches Interface für CLI-Befehle könnte so aussehen, um Befehle zentral zu definieren und zu verwalten:
--
-- data CLICommand = CLICommand
--   { cmdName        :: String             -- Der Name des Befehls (z.B. "--insert")
--   , cmdDescription :: String             -- Eine kurze Beschreibung für Hilfetexte
--   , cmdUsage       :: String             -- Beispiel für die korrekte Benutzung
--   , cmdHandler     :: [String] -> IO ()  -- Die Funktion, die den Befehl ausführt
--   }
--
-- Eine Liste aller verfügbaren Befehle könnte dann so aussehen:
-- allCommands :: [CLICommand]
-- allCommands =
--   [ CLICommand
--       { cmdName        = "--insert"
--       , cmdDescription = "Fügt einen neuen Datensatz hinzu."
--       , cmdUsage       = "--insert <Datei> <ID> <Name> <Wert>"
--       , cmdHandler     = handleInsert
--       }
--   , CLICommand
--       { cmdName        = "--delete"
--       , cmdDescription = "Löscht einen Datensatz anhand der ID."
--       , cmdUsage       = "--delete <Datei> <ID>"
--       , cmdHandler     = handleDelete
--       }
--   , CLICommand
--       { cmdName        = "--help"
--       , cmdDescription = "Zeigt diese Hilfe an."
--       , cmdUsage       = "--help"
--       , cmdHandler     = handleHelp
--       }
--   -- ... weitere Befehle
--   ]
--
-- Die `handleCommand` Funktion könnte dann so umgeschrieben werden, um diese Liste zu nutzen:
-- import Data.List (find)
--
-- handleCommand :: String -> [String] -> IO ()
-- handleCommand cmd args =
--   case find (\c -> cmdName c == cmd) allCommands of
--     Just command -> cmdHandler command args
--     Nothing      -> putStrLn ("Unbekannter Befehl: " ++ cmd)
--
-- Dies würde eine zentralisierte Definition und einfachere Erweiterbarkeit ermöglichen,
-- insbesondere für die Generierung von Hilfetexten oder die Validierung von Befehlen.

 -- Interaktiver modus, dann kann man in der konsole (nach eingabe der datei) direkt mit "help" "version" "update" "insert" "delete" "filter" "query" "stats" "out" interagieren
 -- sachen im ram bearbeiten und dort lassen (nach wahl speichern)

 -- auto backup

 -- pretty print

handleCommand cmd _ = putStrLn ("Unbekannter Befehl: " ++ cmd)


-- Definiere die ANSI-Codes als Strings
red, green, blue, bold, reset :: String
red   = "\ESC[31m"
green = "\ESC[32m"
blue  = "\ESC[34m"
bold  = "\ESC[1m"
reset = "\ESC[0m"

--------------------------------------------------
-- Insert (Task 5)
--------------------------------------------------

handleInsert :: [String] -> IO ()
handleInsert (file:idStr:name:valueStr:_) =
  case (readMaybe idStr :: Maybe Int, readMaybe valueStr :: Maybe Double) of
    (Nothing, _) -> putStrLn $ red ++ "Fehler: ID muss eine Ganzzahl sein." ++ reset
    (_, Nothing) -> putStrLn $ red ++ "Fehler: Wert muss eine Zahl sein." ++ reset
    (Just newId, Just newValue) -> do
      records <- loadRecords file
      let idExists = any (\r -> Record.id r == newId) records
      if idExists
        then putStrLn $ red ++ "Error: Diese ID ist schon vorhanden." ++ reset
        else do
          let newRecord = Record newId name newValue
          let updated   = records ++ [newRecord]
          saveRecords file updated
          putStrLn (green ++ "Neuer Eintrag hinzugefügt: " ++ show newRecord ++ reset)

handleInsert _ = 
    putStrLn "Benutzung: --insert <Datei> <ID> <Name> <Wert>"


--------------------------------------------------
-- Delete (Task 6)
--------------------------------------------------

handleDelete :: [String] -> IO ()
handleDelete (file:idStr:_) =
  case readMaybe idStr :: Maybe Int of
    Nothing -> putStrLn $ red ++ "Fehler: ID muss eine Ganzzahl sein." ++ reset
    Just rid -> do
      records <- loadRecords file
      let updated = filter (\r -> Record.id r /= rid) records
      saveRecords file updated
      putStrLn (green ++ "Eintrag mit ID " ++ show rid ++ " wurde gelöscht (falls vorhanden)." ++ reset)

handleDelete _ =
  putStrLn "Benutzung: --delete <Datei> <ID>"


--------------------------------------------------
-- filter (Task 7)
--------------------------------------------------

handleFilter :: [String] -> IO ()
handleFilter (file:thresholdStr:_) =
  case readMaybe thresholdStr of
    Just threshold -> do
      records <- loadRecords file
      let filtered = filter (\r -> value r > threshold) records
      putStrLn (blue ++ "Einträge mit Wert größer als " ++ show threshold ++ ":" ++ reset)
      mapM_ print filtered
    Nothing -> putStrLn $ red ++ "Fehler: Wert muss eine Zahl sein." ++ reset

handleFilter _ =
  putStrLn "Benutzung: --filter <Datei> <Wert>"


--------------------------------------------------
-- query (Task 8)
--------------------------------------------------

handleQuery :: [String] -> IO ()
handleQuery (file:searchStr:_) = do
  records <- loadRecords file
  let searchLower = map toLower searchStr
  let found = filter (\r -> isInfixOf searchLower (map toLower (Record.name r))) records
  putStrLn (blue ++ "Einträge mit Name, der \"" ++ searchStr ++ "\" enthält:" ++ reset)
  mapM_ print found

handleQuery _ =
  putStrLn "Benutzung: --query <Datei> <Suchbegriff>"



--------------------------------------------------
-- stats (New)
--------------------------------------------------

handleStats :: [String] -> IO ()
handleStats (file:_) = do
  records <- loadRecords file
  if null records
    then putStrLn $ red ++ "Keine Daten vorhanden." ++ reset
    else do
      let values   = map Record.value records
      let total    = sum values
      let count    = fromIntegral (length values) :: Double
      let average  = total / count
      let minVal   = minimum values
      let maxVal   = maximum values

      putStrLn (blue ++ "Statistik:" ++ reset)
      putStrLn ("  Anzahl:       " ++ show (length values))
      putStrLn ("  Summe:        " ++ show total)
      putStrLn ("  Durchschnitt: " ++ show average)
      putStrLn ("  Min:          " ++ show minVal)
      putStrLn ("  Max:          " ++ show maxVal)

handleStats _ =
      putStrLn "Benutzung: --stats <Datei>"


--------------------------------------------------
-- out (Task 9)
--------------------------------------------------

handleOut :: [String] -> IO ()
handleOut ("-":file:_) = do
  json <- readFile file
  putStrLn json

handleOut (outfile:file:_) = do
  records <- loadRecords file
  saveRecords outfile records
  putStrLn $ blue ++ "Ausgabe gespeichert in: " ++ outfile ++ reset

handleOut _ =
  putStrLn "Benutzung: --out <Datei> <JSON-Datei>   oder   --out - <Datei>"

--------------------------------------------------
-- help
--------------------------------------------------

handleHelp :: [String] -> IO ()
handleHelp _ = do
    putStrLn "Verfügbare Befehle:"
    putStrLn ""
    putStrLn "  --insert <Datei> <ID> <Name> <Wert>"
    putStrLn "      Fügt einen neuen Datensatz ein."
    putStrLn ""
    putStrLn "  --delete <Datei> <ID>"
    putStrLn "      Löscht einen Datensatz anhand seiner ID."
    putStrLn ""
    putStrLn "  --filter <Datei> <Wert>"
    putStrLn "      Zeigt alle Einträge mit value > Wert."
    putStrLn ""
    putStrLn "  --query <Datei> <Text>"
    putStrLn "      Sucht nach Namen, die den Text enthalten."
    putStrLn ""
    putStrLn "  --stats <Datei>"
    putStrLn "      Berechnet Summe, Durchschnitt, Minimum und Maximum."
    putStrLn ""
    putStrLn "  --out - <Datei>"
    putStrLn "      Ausgabe auf die Konsole."
    putStrLn ""
    putStrLn "  --out <Zieldatei> <Datei>"
    putStrLn "      Speichert JSON in neuer Datei."
    putStrLn ""
    putStrLn "  --help"
    putStrLn "      Zeigt diese Hilfe an."


--------------------------------------------------
-- version
--------------------------------------------------
handleVersion :: [String] -> IO ()
handleVersion _ = do
    putStrLn "CLI-Tool Version 1.0.0"


--------------------------------------------------
-- Exporte
--------------------------------------------------

insert :: [String] -> IO ()
insert = handleInsert

delete :: [String] -> IO ()
delete = handleDelete

filterR :: [String] -> IO ()
filterR = handleFilter

query :: [String] -> IO ()
query = handleQuery

stats :: [String] -> IO ()
stats = handleStats

out :: [String] -> IO ()
out = handleOut


help ::IO ()
help = handleHelp []

version :: IO ()
version = handleVersion []
