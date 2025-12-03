-- Modul für die Verarbeitung der CLI-Befehle
module CLI
  ( handleCommand,
    parseCommand, -- Exported for Shell
    insert,
    update, -- Exported wrapper
    delete,
    filterR,
    query,
    stats,
    save,
    printR,
    help,
    version,
    
    -- Export logic functions for Shell
    logicInsert,
    logicUpdate, -- Exported logic
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

-- Definiere die ANSI-Codes als Strings
red, green, blue, bold, reset :: String
red   = "\ESC[31m"
green = "\ESC[32m"
blue  = "\ESC[34m"
bold  = "\ESC[1m"
reset = "\ESC[0m"

-- | Central parsing logic
parseCommand :: String -> [String] -> Either String Command
parseCommand "insert" (idStr:name:valStr:_) =
  case (readMaybe idStr, readMaybe valStr) of
    (Just i, Just v) -> Right (Insert i name v)
    (Nothing, _)     -> Left "Fehler: ID muss eine Ganzzahl sein."
    (_, Nothing)     -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "insert" _ = Left "Benutzung: insert <ID> <Name> <Wert>"

parseCommand "update" (idStr:name:valStr:_) =
  case (readMaybe idStr, readMaybe valStr) of
    (Just i, Just v) -> Right (Update i name v)
    (Nothing, _)     -> Left "Fehler: ID muss eine Ganzzahl sein."
    (_, Nothing)     -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "update" _ = Left "Benutzung: update <ID> <NeuerName> <NeuerWert>"

parseCommand "delete" (idStr:_) =
  case readMaybe idStr of
    Just i  -> Right (Delete i)
    Nothing -> Left "Fehler: ID muss eine Ganzzahl sein."
parseCommand "delete" _ = Left "Benutzung: delete <ID>"

parseCommand "filter" (valStr:_) =
  case readMaybe valStr of
    Just v  -> Right (Filter v)
    Nothing -> Left "Fehler: Wert muss eine Zahl sein."
parseCommand "filter" _ = Left "Benutzung: filter <Wert>"

parseCommand "query" (str:_) = Right (Query str)
parseCommand "query" _       = Left "Benutzung: query <Suchbegriff>"

parseCommand "stats" _ = Right Stats
parseCommand "list" _  = Right List -- Shell alias
parseCommand "print" _ = Right Print

parseCommand "save" (file:_) = Right (Save (Just file))
parseCommand "save" []       = Right (Save Nothing)

parseCommand "help" _    = Right Help
parseCommand "version" _ = Right Version
parseCommand "quit" _    = Right Quit
parseCommand "exit" _    = Right Quit

-- Handle CLI-specific flags (remove -- prefix)
parseCommand ('-':'-':cmd) args = parseCommand cmd args
parseCommand cmd _ = Right (Unknown cmd)


-- | Executes a parsed command (CLI context)
handleCommand :: String -> [String] -> IO ()
handleCommand cmd args = 
  let cleanCmd = dropWhile (== '-') cmd
  in case cleanCmd of
       "help"    -> handleHelp
       "version" -> handleVersion
       _ -> if null args 
            then putStrLn "Fehler: Keine Datei angegeben."
            else do
              let file = head args
                  restArgs = tail args
              
              case parseCommand cleanCmd restArgs of
                Left err -> putStrLn $ red ++ err ++ reset
                Right command -> executeCommand file command

executeCommand :: FilePath -> Command -> IO ()
executeCommand file cmd = do
  case cmd of
    Insert i n v -> do
      records <- loadRecords file
      case logicInsert i n v records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Neuer Eintrag hinzugefügt: " ++ show (Record i n v) ++ reset)
    
    Update i n v -> do
      records <- loadRecords file
      case logicUpdate i n v records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Eintrag aktualisiert: " ++ show (Record i n v) ++ reset)

    Delete i -> do
      records <- loadRecords file
      case logicDelete i records of
        Left err -> putStrLn $ red ++ err ++ reset
        Right updated -> do
          saveRecords file updated
          putStrLn (green ++ "Erfolg: Eintrag mit ID " ++ show i ++ " wurde gelöscht." ++ reset)

    Filter v -> do
      records <- loadRecords file
      let filtered = logicFilter v records
      putStrLn (blue ++ "Einträge mit Wert größer als " ++ show v ++ ":" ++ reset)
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
                      Just f -> f
                      Nothing -> file -- Default to current file if no other given
      records <- loadRecords file
      saveRecords outfile records
      putStrLn $ blue ++ "Ausgabe gespeichert in: " ++ outfile ++ reset

    Unknown c -> putStrLn ("Befehl nicht erkannt: " ++ c ++ ". Nutze '--help' für eine Übersicht.")
    
    _ -> putStrLn "Dieser Befehl ist im CLI-Modus nicht verfügbar oder benötigt keine Datei."

--------------------------------------------------
-- Logic Functions
--------------------------------------------------

logicInsert :: Int -> String -> Double -> [Record] -> Either String [Record]
logicInsert newId name val records =
  if any (\r -> Types.id r == newId) records
    then Left "Fehler: Diese ID ist schon vorhanden."
    else Right (records ++ [Record newId name val])

logicUpdate :: Int -> String -> Double -> [Record] -> Either String [Record]
logicUpdate uid name val records =
  if any (\r -> Types.id r == uid) records
    then Right (map (\r -> if Types.id r == uid then Record uid name val else r) records)
    else Left ("Fehler: ID " ++ show uid ++ " nicht gefunden.")

logicDelete :: Int -> [Record] -> Either String [Record]
logicDelete rid records = 
  if any (\r -> Types.id r == rid) records
    then Right (filter (\r -> Types.id r /= rid) records)
    else Left ("Fehler: ID " ++ show rid ++ " nicht gefunden.")

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
-- Legacy Handlers
--------------------------------------------------

handleHelp :: IO ()
handleHelp = do
    putStrLn "Verfügbare Befehle:"
    putStrLn ""
    putStrLn "  --insert <Datei> <ID> <Name> <Wert>"
    putStrLn "      Fügt einen neuen Datensatz ein."
    putStrLn ""
    putStrLn "  --update <Datei> <ID> <NeuerName> <NeuerWert>"
    putStrLn "      Aktualisiert einen bestehenden Datensatz."
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
    putStrLn "  --print <Datei>"
    putStrLn "      Ausgabe auf die Konsole (formatiert)."
    putStrLn ""
    putStrLn "  --save <Quelldatei> <Zieldatei>"
    putStrLn "      Speichert JSON in neuer Datei."
    putStrLn ""
    putStrLn "  --help"
    putStrLn "      Zeigt diese Hilfe an."

handleVersion :: IO ()
handleVersion = do
    putStrLn "Haskell JSON Manager v1.0.0"


--------------------------------------------------
-- Exporte
--------------------------------------------------

insert :: [String] -> IO ()
insert args = handleCommand "--insert" args

update :: [String] -> IO ()
update args = handleCommand "--update" args

delete :: [String] -> IO ()
delete args = handleCommand "--delete" args

filterR :: [String] -> IO ()
filterR args = handleCommand "--filter" args

query :: [String] -> IO ()
query args = handleCommand "--query" args

stats :: [String] -> IO ()
stats args = handleCommand "--stats" args

printR :: [String] -> IO ()
printR args = handleCommand "--print" args

save :: [String] -> IO ()
save args = handleCommand "--save" args

help ::IO ()
help = handleHelp

version :: IO ()
version = handleVersion