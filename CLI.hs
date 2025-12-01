-- Modul für die Verarbeitung der CLI-Befehle
module CLI
  ( handleCommand,
    insert,
    delete,
    filterR,
    query,
    stats,
    out
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

handleCommand cmd _ = putStrLn ("Unbekannter Befehl: " ++ cmd)


--------------------------------------------------
-- Insert (Task 5)
--------------------------------------------------

handleInsert :: [String] -> IO ()
handleInsert (file:idStr:name:valueStr:_) =
  case (readMaybe idStr :: Maybe Int, readMaybe valueStr :: Maybe Double) of
    (Nothing, _) -> putStrLn "Fehler: ID muss eine Ganzzahl sein."
    (_, Nothing) -> putStrLn "Fehler: Wert muss eine Zahl sein."
    (Just newId, Just newValue) -> do
      records <- loadRecords file
      let idExists = any (\r -> Record.id r == newId) records
      if idExists
        then putStrLn "Error: Diese ID ist schon vorhanden."
        else do
          let newRecord = Record newId name newValue
          let updated   = records ++ [newRecord]
          saveRecords file updated
          putStrLn ("Neuer Eintrag hinzugefügt: " ++ show newRecord)

handleInsert _ =
--   putStrLn "Benutzung: --insert <Datei> <ID> <Name> <Wert>"
    putStrLn "In GHCI: insert [\"datei\", \"id\", \"name\", \"value\"]"


--------------------------------------------------
-- Delete (Task 6)
--------------------------------------------------

handleDelete :: [String] -> IO ()
handleDelete (file:idStr:_) =
  case readMaybe idStr :: Maybe Int of
    Nothing -> putStrLn "Fehler: ID muss eine Ganzzahl sein."
    Just rid -> do
      records <- loadRecords file
      let updated = filter (\r -> Record.id r /= rid) records
      saveRecords file updated
      putStrLn ("Eintrag mit ID " ++ show rid ++ " wurde gelöscht (falls vorhanden).")

handleDelete _ =
--   putStrLn "Benutzung: --delete <Datei> <ID>"
    putStrLn "In GHCI: delete [\"datei\", \"id\"]"


--------------------------------------------------
-- filter (Task 7)
--------------------------------------------------

handleFilter :: [String] -> IO ()
handleFilter (file:thresholdStr:_) =
  case readMaybe thresholdStr of
    Just threshold -> do
      records <- loadRecords file
      let filtered = filter (\r -> value r > threshold) records
      putStrLn ("Einträge mit Wert größer als " ++ show threshold ++ ":")
      mapM_ print filtered
    Nothing -> putStrLn "Fehler: Wert muss eine Zahl sein."

handleFilter _ =
--   putStrLn "Benutzung: --filter <Datei> <Wert>"
    putStrLn "In GHCI: filter ["datei", "threshold"]"


--------------------------------------------------
-- query (Task 8)
--------------------------------------------------

handleQuery :: [String] -> IO ()
handleQuery (file:searchStr:_) = do
  records <- loadRecords file
  let searchLower = map toLower searchStr
  let found = filter (\r -> isInfixOf searchLower (map toLower (Record.name r))) records
  putStrLn ("Einträge mit Name, der \"" ++ searchStr ++ "\" enthält:")
  mapM_ print found

handleQuery _ =
--   putStrLn "Benutzung: --query <Datei> <Suchbegriff>"
    putStrLn "In GHCI: query ["datei", "searchStr"]"


--------------------------------------------------
-- stats (New)
--------------------------------------------------

handleStats :: [String] -> IO ()
handleStats (file:_) = do
  records <- loadRecords file
  if null records
    then putStrLn "Keine Daten vorhanden."
    else do
      let values   = map Record.value records
      let total    = sum values
      let count    = fromIntegral (length values) :: Double
      let average  = total / count
      let minVal   = minimum values
      let maxVal   = maximum values

      putStrLn "Statistik:"
      putStrLn ("  Anzahl:       " ++ show (length values))
      putStrLn ("  Summe:        " ++ show total)
      putStrLn ("  Durchschnitt: " ++ show average)
      putStrLn ("  Min:          " ++ show minVal)
      putStrLn ("  Max:          " ++ show maxVal)

handleStats _ =
--   putStrLn "Benutzung: --stats <Datei>"
    putStrLn "In GHCI: stats ["datei"]"


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
  putStrLn ("Ausgabe gespeichert in: " ++ outfile)

handleOut _ =
--   putStrLn "Benutzung: --out <Datei> <JSON-Datei>   oder   --out - <Datei>"
    putStrLn "In GHCI: out [\"datei\", \"jsondatei\"]   oder   out [\"-\", \"jsondatei\"]"


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
