-- | CLI.hs - Command Line Interface Verarbeitung
--
-- Dieses Modul enthält:
-- 1. parseCommand: Wandelt Strings in Command-Typen um
-- 2. handleCommand/executeCommand: Führt Commands aus (IO)
-- 3. logic*-Funktionen
-- 4. GHCI Convenience Functions: Zum direkten Testen in GHCI
--
module CLI
  ( handleCommand,
    parseCommand,
    insert,
    update,
    delete,
    filterRecords,
    query,
    stats,
    save,
    printRecords,
    help,
    version,
    
    -- Logic-Funktionen für Shell
    logicInsert,
    logicUpdate,
    logicDelete,
    logicFilter,
    logicQuery,
    logicStats

  )
where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

import DataHandler
import Types

-- | Zentrale Parsing-Logik: String -> Command
parseCommand :: String -> [String] -> Either String Command
-- Insert: Braucht ID, Name, Wert
parseCommand "insert" (idStr:name:valStr:_) =
  case (readMaybe idStr, readMaybe valStr) of
    (Just recordId, Just value) -> Right (Insert recordId name value)
    (Nothing, _)     -> Left "Fehler: ID muss eine Ganzzahl sein."
    (_, Nothing)     -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "insert" _ = Left "Benutzung: insert <ID> <Name> <Wert>"

-- Update: Braucht ID, NeuerName, NeuerWert
parseCommand "update" (idStr:name:valStr:_) =
  case (readMaybe idStr, readMaybe valStr) of
    (Just recordId, Just value) -> Right (Update recordId name value)
    (Nothing, _)     -> Left "Fehler: ID muss eine Ganzzahl sein."
    (_, Nothing)     -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "update" _ = Left "Benutzung: update <ID> <NeuerName> <NeuerWert>"

-- Delete: Braucht ID
parseCommand "delete" (idStr:_) =
  case readMaybe idStr of
    Just recordId  -> Right (Delete recordId)
    Nothing -> Left "Fehler: ID muss eine Ganzzahl sein."
parseCommand "delete" _ = Left "Benutzung: delete <ID>"

-- Filter: Braucht Wert
parseCommand "filter" (valStr:_) =
  case readMaybe valStr of
    Just minValue  -> Right (Filter minValue)
    Nothing -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "filter" _ = Left "Benutzung: filter <Wert>"

-- Query: Braucht Suchbegriff
parseCommand "query" (str:_) = Right (Query str)
parseCommand "query" _       = Left "Benutzung: query <Suchbegriff>"

-- Save: Optionaler Dateiname
parseCommand "save" (file:_) = Right (Save (Just file))
parseCommand "save" []       = Right (Save Nothing)

-- Befehle ohne Argumente
parseCommand "stats" _ = Right Stats
parseCommand "list" _  = Right List -- Shell alias
parseCommand "print" _ = Right Print

parseCommand "help" _    = Right Help
parseCommand "version" _ = Right Version
parseCommand "quit" _    = Right Quit
parseCommand "exit" _    = Right Quit

-- Handle CLI-specific flags (remove -- prefix)
-- Beispiel: --insert wird zu insert
-- | Pattern-Matching für Befehle mit '--' Präfix
parseCommand ('-':'-':cmd) args = parseCommand cmd args
parseCommand cmd _ = Right (Unknown cmd)


-- | Führt einen CLI-Befehl aus
handleCommand :: String -> [String] -> IO ()
handleCommand cmd args = 
  let cleanCmd = dropWhile (== '-') cmd
  in case cleanCmd of
       "help"    -> handleHelp
       "version" -> handleVersion
       _ -> if null args 
            then putStrLn $ red ++ "Fehler: Keine Datei angegeben." ++ reset
            else do
              let file = head args
                  restArgs = tail args
              
              case parseCommand cleanCmd restArgs of
                Left err -> putStrLn $ red ++ err ++ reset
                Right command -> executeCommand file command

-- | Führt einen Command im CLI-Modus aus
executeCommand :: FilePath -> Command -> IO ()
executeCommand file cmd = do
  case cmd of
    Insert recordId name value -> do
      records <- loadRecords file
      case logicInsert recordId name value records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Neuer Eintrag hinzugefügt: " ++ show (Record recordId name value) ++ reset)
    
    Update recordId name value -> do
      records <- loadRecords file
      case logicUpdate recordId name value records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Eintrag aktualisiert: " ++ show (Record recordId name value) ++ reset)

    Delete recordId -> do
      records <- loadRecords file
      case logicDelete recordId records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Eintrag mit ID " ++ show recordId ++ " wurde gelöscht." ++ reset)

    Filter minValue -> do
      records <- loadRecords file
      let filtered = logicFilter minValue records
      putStrLn (blue ++ "Einträge mit Wert größer als " ++ show minValue ++ ":" ++ reset)
      B.putStrLn (encodePretty filtered)

    Query str -> do
      records <- loadRecords file
      let found = logicQuery str records
      putStrLn (blue ++ "Einträge mit Name, der \"" ++ str ++ "\" enthält:" ++ reset)
      B.putStrLn (encodePretty found)

    Stats -> do
      records <- loadRecords file
      putStrLn (blue ++ logicStats records ++ reset)

    Print -> do
      records <- loadRecords file
      B.putStrLn (encodePretty records)

    Save maybeOutfile -> do
      let outfile = case maybeOutfile of
                      Just targetFile -> targetFile
                      Nothing -> file -- Default to current file if no other given
      records <- loadRecords file
      saveRecords outfile records
      putStrLn $ blue ++ "Ausgabe gespeichert in: " ++ outfile ++ reset

    Unknown unknownCmd -> putStrLn $ red ++ "Befehl nicht erkannt: " ++ unknownCmd ++ ". Nutze '--help' für eine Übersicht." ++ reset
    
    _ -> putStrLn $ red ++ "Dieser Befehl ist im CLI-Modus nicht verfügbar oder benötigt keine Datei." ++ reset

--------------------------------------------------
-- Logic Functions (PURE - kein IO!)
--------------------------------------------------
-- Diese Funktionen sind "pure": Sie haben keine Seiteneffekte.
-- Hier kommen Daten rein, da kommen Daten raus.

-- | Fügt einen neuen Record ein
-- Prüft erst, ob die ID schon existiert (Duplikate verhindern)
-- 
-- any :: (a -> Bool) -> [a] -> Bool
-- any prüft, ob mindestens ein Element die Bedingung erfüllt
logicInsert :: Int -> String -> Double -> [Record] -> Either String [Record]
logicInsert newId name value records =
  if any (\record -> Types.id record == newId) records
    then Left "Fehler: Diese ID ist schon vorhanden."
    else Right (records ++ [Record newId name value])  -- Neuen Record anhängen

-- | Aktualisiert einen bestehenden Record
-- map wendet eine Funktion auf jedes Element an
-- Wenn die ID passt -> neuer Record, sonst -> alter Record
logicUpdate :: Int -> String -> Double -> [Record] -> Either String [Record]
logicUpdate recordId name value records =
  if any (\record -> Types.id record == recordId) records
    then Right (map (\record -> if Types.id record == recordId 
                                then Record recordId name value  -- Ersetzen
                                else record) records)            -- Behalten
    else Left ("Fehler: ID " ++ show recordId ++ " nicht gefunden.")

-- | Löscht einen Record anhand der ID
-- filter behält nur Elemente, die die Bedingung erfüllen
logicDelete :: Int -> [Record] -> Either String [Record]
logicDelete recordId records = 
  if any (\record -> Types.id record == recordId) records
    then Right (filter (\record -> Types.id record /= recordId) records)
    else Left ("Fehler: ID " ++ show recordId ++ " nicht gefunden.")

-- | Filtert Records nach Wert (größer als threshold)
logicFilter :: Double -> [Record] -> [Record]
logicFilter threshold records = filter (\record -> Types.value record > threshold) records

-- | Sucht Records nach Name (case-insensitive)
-- isInfixOf prüft, ob ein String in einem anderen enthalten ist
logicQuery :: String -> [Record] -> [Record]
logicQuery searchStr records =
  let searchLower = map toLower searchStr  -- toLower = case-insensitive
  in filter (\record -> isInfixOf searchLower (map toLower (Types.name record))) records

-- | Berechnet Statistiken über alle Records
logicStats :: [Record] -> String
logicStats records =
  if null records
    then red ++ "Keine Daten vorhanden." ++ reset
    else
      let values  = map Types.value records
          total   = sum values
          count   = fromIntegral (length values) :: Double
          average = total / count
          minVal  = minimum values
          maxVal  = maximum values
      in unlines  -- Verbindet Strings mit Zeilenumbrüchen
          [ green ++ "Statistik:" ++ reset
          , "  Anzahl:       " ++ show (length values)
          , "  Summe:        " ++ show total
          , "  Durchschnitt: " ++ show average
          , "  Min:          " ++ show minVal
          , "  Max:          " ++ show maxVal
          ]

--------------------------------------------------
-- CLI Wrapper Functions (für Help und Version)
--------------------------------------------------

handleHelp :: IO ()
handleHelp = do
    putStrLn $ blue ++ bold ++ "Verfügbare Befehle:" ++ reset
    putStrLn ""
    putStrLn $ green ++ "  --insert <Datei> <ID> <Name> <Wert>" ++ reset
    putStrLn "      Fügt einen neuen Datensatz ein."
    putStrLn ""
    putStrLn $ green ++ "  --update <Datei> <ID> <NeuerName> <NeuerWert>" ++ reset
    putStrLn "      Aktualisiert einen bestehenden Datensatz."
    putStrLn ""
    putStrLn $ green ++ "  --delete <Datei> <ID>" ++ reset
    putStrLn "      Löscht einen Datensatz anhand seiner ID."
    putStrLn ""
    putStrLn $ green ++ "  --filter <Datei> <Wert>" ++ reset
    putStrLn "      Zeigt alle Einträge mit value > Wert."
    putStrLn ""
    putStrLn $ green ++ "  --query <Datei> <Text>" ++ reset
    putStrLn "      Sucht nach Namen, die den Text enthalten."
    putStrLn ""
    putStrLn $ green ++ "  --stats <Datei>" ++ reset
    putStrLn "      Berechnet Summe, Durchschnitt, Minimum und Maximum."
    putStrLn ""
    putStrLn $ green ++ "  --print <Datei>" ++ reset
    putStrLn "      Ausgabe auf die Konsole (formatiert)."
    putStrLn ""
    putStrLn $ green ++ "  --save <Quelldatei> <Zieldatei>" ++ reset
    putStrLn "      Speichert JSON in neuer Datei."
    putStrLn ""
    putStrLn $ green ++ "  --help" ++ reset
    putStrLn "      Zeigt diese Hilfe an."

handleVersion :: IO ()
handleVersion = do
    putStrLn $ blue ++ "Haskell JSON Manager v1.0.0" ++ reset


--------------------------------------------------
-- GHCI Functions (zum direkten Aufrufen in GHCI)
--------------------------------------------------

insert :: [String] -> IO ()
insert args = handleCommand "--insert" args

update :: [String] -> IO ()
update args = handleCommand "--update" args

delete :: [String] -> IO ()
delete args = handleCommand "--delete" args

filterRecords :: [String] -> IO ()
filterRecords args = handleCommand "--filter" args

query :: [String] -> IO ()
query args = handleCommand "--query" args

stats :: [String] -> IO ()
stats args = handleCommand "--stats" args

printRecords :: [String] -> IO ()
printRecords args = handleCommand "--print" args

save :: [String] -> IO ()
save args = handleCommand "--save" args

help :: IO ()
help = handleHelp

version :: IO ()
version = handleVersion