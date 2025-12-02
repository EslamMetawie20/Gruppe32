-- Modul für die Verarbeitung der CLI-Befehle
module CLI
  ( handleCommand,
    insert,
    delete,
    filterR,
    query,
    stats,
    out,
    
    -- Export logic functions for Shell
    logicInsert,
    logicDelete,
    logicFilter,
    logicQuery,
    logicStats
  )
where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

import DataHandler
import Types

-- Entscheidet, welcher Befehl ausgeführt wird
handleCommand :: String -> [String] -> IO ()
handleCommand "--insert" args = handleInsert args
handleCommand "--delete" args = handleDelete args
handleCommand "--filter" args = handleFilter args
handleCommand "--query"  args = handleQuery args
handleCommand "--stats"  args = handleStats args
handleCommand "--out"    args = handleOut args

-- handleCommand "--help"   args = handleHelp args
-- handleCommand "--version"args = handleVersion args
-- handleCommand "--update" args = handleUpdate args

-- weitere ideen:
--  
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
-- Logic Functions (Pure / In-Memory)
--------------------------------------------------

logicInsert :: Int -> String -> Double -> [Record] -> Either String [Record]
logicInsert newId name val records =
  if any (\r -> Types.id r == newId) records
    then Left "Error: Diese ID ist schon vorhanden."
    else Right (records ++ [Record newId name val])

logicDelete :: Int -> [Record] -> [Record]
logicDelete rid records = filter (\r -> Types.id r /= rid) records

logicFilter :: Double -> [Record] -> [Record]
logicFilter threshold records = filter (\r -> Types.value r > threshold) records

logicQuery :: String -> [Record] -> [Record]
logicQuery searchStr records =
  let searchLower = map toLower searchStr
  in filter (\r -> isInfixOf searchLower (map toLower (Types.name r))) records

logicStats :: [Record] -> String
logicStats records =
  if null records
    then "Keine Daten vorhanden."
    else
      let values   = map Types.value records
          total    = sum values
          count    = fromIntegral (length values) :: Double
          average  = total / count
          minVal   = minimum values
          maxVal   = maximum values
      in unlines
          [ "Statistik:"
          , "  Anzahl:       " ++ show (length values)
          , "  Summe:        " ++ show total
          , "  Durchschnitt: " ++ show average
          , "  Min:          " ++ show minVal
          , "  Max:          " ++ show maxVal
          ]

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
      case logicInsert newId name newValue records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Neuer Eintrag hinzugefügt: " ++ show (Record newId name newValue) ++ reset)

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
      let updated = logicDelete rid records
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
      let filtered = logicFilter threshold records
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
  let found = logicQuery searchStr records
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
  putStrLn (blue ++ logicStats records ++ reset)

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
