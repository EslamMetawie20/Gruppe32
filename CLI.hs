-- Modul für die Verarbeitung der CLI-Befehle
module CLI 
    ( handleCommand
    , insert
    , delete
    , filterR   -- die Funktion heißt filterR, um Konflikte mit Prelude.filter zu vermeiden
    , query
    , out
    ) where

import Record
import DataHandler

import Data.Char (toLower) -- Query: non case-sensitive search
import Text.Read (readMaybe) -- Fehlerbehandlung beim lesen

-- Entscheidet, welcher Befehl ausgeführt wird
handleCommand :: String -> [String] -> IO ()
handleCommand "--insert" args = handleInsert args
handleCommand "--delete" args = handleDelete args         
handleCommand "--filter" args = handleFilter args
handleCommand "--query"  args = handleQuery args
handleCommand "--out" args = handleOut args

                                                            
handleCommand cmd _           = putStrLn ("Unbekannter Befehl: " ++ cmd)


--------------------------------------------------
-- Insert (Task 5)
--------------------------------------------------

handleInsert :: [String] -> IO ()
handleInsert (file:idStr:name:valueStr:_) = do 
    -- ID und Wert von Text zu Zahlen umwandeln
    let newId    = read idStr
    let newValue = read valueStr  
    
    -- Alte Einträge laden
    records <- loadRecords file     

    -- Duplicate check with guards
    let idExists = any checkId records
        checkId r = Record.id r == newId

    if idExists 
        then putStrLn "Error : diese ID ist schon da"
        else do 
            -- Neuen Eintrag erstellen
            let newRecord = Record newId name newValue   
            
            -- Eintrag an Liste anhängen
            let updated = records ++ [newRecord]  
            
            -- Datei aktualisieren
            saveRecords file updated

            putStrLn ("Neuer Eintrag hinzugefügt: " ++ show newRecord)

-- Wenn Argumente fehlen
handleInsert _ =
    putStrLn "Benutzung: --insert <Datei> <ID> <Name> <Wert>"


--------------------------------------------------
-- Delete (Task 6)
--------------------------------------------------

handleDelete :: [String] -> IO ()
handleDelete (file:idStr:_) = do
    -- ID konvertieren
    let rid = read idStr :: Int

    -- Einträge laden
    records <- loadRecords file

    -- Eintrag entfernen
    let updated = filter (\r -> Record.id r /= rid) records

    -- Speichern
    saveRecords file updated

    putStrLn ("Eintrag mit ID " ++ show rid ++ " wurde gelöscht (falls vorhanden).")

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
            putStrLn ("Eintreage mit Wert groeßer als " ++ show threshold ++ ":")
            mapM_ print filtered
        Nothing -> putStrLn "Fehler: Wert muss eine Zahl sein."

handleFilter _ = putStrLn "Benutzung: --filter <Datei> <Wert>"

--------------------------------------------------
-- query (Task 8)
--------------------------------------------------

handleQuery :: [String] -> IO ()
handleQuery (file:searchStr:_) = do
    -- Einträge laden
    records <- loadRecords file

    -- Gefundene Einträge anzeigen (non case-sensitive)
    let searchLower = map toLower searchStr
    let found = filter (\r -> contains searchLower (map toLower (Record.name r))) records

    putStrLn ("Eintraege mit Name, der \"" ++ searchStr ++ "\" enthaelt:")
    mapM_ print found

handleQuery _ = putStrLn "Benutzung: --query <Datei> <Suchbegriff>"

-- Hilfsfunktion: Prüft, ob sub in s enthalten ist
contains :: String -> String -> Bool
contains [] _ = True
contains _ [] = False
contains sub (c:cs)
    | startsWith sub (c:cs) = True
    | otherwise = contains sub cs

-- Hilfsfunktion: Prüft, ob s mit prefix beginnt
startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (p:ps) (s:ss)
    | p == s    = startsWith ps ss
    | otherwise = False




--------------------------------------------------
-- out (Task 9)
--------------------------------------------------



handleOut :: [String] -> IO ()
handleOut ("-":file:_) = do
    records <- loadRecords file
    -- JSON erzeugen, indem wir temporär in eine Datei speichern
    let tempFile = "__temp_output.json"
    saveRecords tempFile records      -- schreibt echtes JSON
    json <- readFile tempFile
    putStrLn json                    -- JSON auf Konsole ausgeben

handleOut (outfile:file:_) = do
    records <- loadRecords file
    saveRecords outfile records      -- direktes JSON in Datei
    putStrLn ("Ausgabe gespeichert in: " ++ outfile)

handleOut _ =
    putStrLn "Benutzung: --out <Datei> <JSON-Datei>   oder   --out - <Datei>"

    
insert :: [String] -> IO ()
insert = handleInsert

delete :: [String] -> IO ()
delete = handleDelete

filterR :: [String] -> IO ()
filterR = handleFilter

query :: [String] -> IO ()
query = handleQuery

out :: [String] -> IO ()
out = handleOut

