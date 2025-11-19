-- Modul für die Verarbeitung der CLI-Befehle
module CLI (handleCommand) where

import Record
import DataHandler

-- Entscheidet, welcher Befehl ausgeführt wird
handleCommand :: String -> [String] -> IO ()
handleCommand "--insert" args = handleInsert args
handleCommand "--delete" args = handleDelete args         
                                                        
                                                        -- @Gry  TODO  filter und fuery müssen hier augerufen werden 
                                                            
handleCommand cmd _           = putStrLn ("Unbekannter Befehl: " ++ cmd)


--------------------------------------------------
-- Insert (Task 5)
--------------------------------------------------

handleInsert :: [String] -> IO ()
handleInsert (file:idStr:name:valueStr:_) = do 
    -- ID und Wert von Text zu Zahlen umwandeln
    let newId    = read idStr
    let newValue = read valueStr  -- Alte Einträge laden
    records <- loadRecords file     -- Neuen Eintrag erstellen
    let newRecord = Record newId name newValue   -- Eintrag an Liste anhängen
    let updated = records ++ [newRecord]  -- Datei aktualisieren
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

-- @Gry

--------------------------------------------------
-- query (Task 8)
--------------------------------------------------

-- @Gry