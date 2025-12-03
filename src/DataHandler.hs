-- | DataHandler.hs - Datei-Ein/Ausgabe für JSON-Dateien
-- 
-- Dieses Modul kümmert sich um:
-- - Laden von JSON-Dateien in [Record]
-- - Speichern von [Record] als JSON
-- - Automatisches Backup bei jedem Speichern
--
-- Wichtig: Alle IO-Fehler werden hier abgefangen und behandelt!
module DataHandler
  ( loadRecords   -- Lädt Records aus JSON-Datei
  , saveRecords   -- Speichert Records in JSON-Datei
  ) where

import qualified Data.ByteString.Lazy as B      -- Effizientes Lesen/Schreiben von Dateien
import Data.Aeson.Encode.Pretty (encodePretty)  -- Schöne JSON-Formatierung

import Data.Aeson (decodeFileStrict, encodeFile)  -- JSON Encoding/Decoding
import Types (Record, red, green, blue, reset)

-- Für Backup-Dateipfad-Manipulation
import System.FilePath (takeDirectory, takeBaseName, takeExtension, (</>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Control.Exception (try, IOException)  -- Für Exception-Handling
import System.Exit (exitFailure)             -- Programm mit Fehlercode beenden

-- | Lädt alle Records aus einer JSON-Datei
--
-- Rückgabe: IO [Record] - Eine IO-Aktion, die eine Liste von Records liefert
--
-- Fehlerbehandlung mit 'try':
-- - try fängt IOExceptions ab (Datei nicht gefunden, keine Berechtigung, etc.)
-- - Gibt Either IOException (Maybe [Record]) zurück:
--   - Left err        -> IOException aufgetreten
--   - Right Nothing   -> Datei geladen, aber JSON ungültig
--   - Right (Just rs) -> Erfolgreich geladen
loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    -- try :: IO a -> IO (Either IOException a)
    -- Wickelt die IO-Aktion ein und fängt Exceptions ab
    result <- try (decodeFileStrict path) :: IO (Either IOException (Maybe [Record]))
    case result of
        Left err -> do
            -- IOException: Datei konnte nicht gelesen werden
            putStrLn $ red ++ "Fehler beim Lesen der Datei: " ++ show err ++ reset
            exitFailure
        Right Nothing -> do
            -- Datei gelesen, aber JSON-Parsing fehlgeschlagen
            putStrLn $ red ++ "Fehler: JSON-Format ungültig oder Datei leer." ++ reset
            exitFailure
        Right (Just records) -> 
            -- Alles OK: Records zurückgeben
            return records

-- | Speichert eine Liste von Records als formatiertes JSON
--
-- encodePretty erzeugt schön formatiertes JSON mit Einrückung
-- Nach dem Speichern wird automatisch ein Backup erstellt
saveRecords :: FilePath -> [Record] -> IO ()
saveRecords path records = do
    result <- try (B.writeFile path (encodePretty records)) :: IO (Either IOException ())
    case result of
        Left err -> do
            putStrLn $ red ++ "Fehler beim Speichern: " ++ show err ++ reset
            exitFailure
        Right _ -> do
            -- Erfolgreich gespeichert -> Backup erstellen
            makeBackup path records
            return ()

-- | Erstellt eine Backup-Datei mit Zeitstempel im Dateinamen
--
-- Beispiel: data.json -> data_2025-12-03_14-30-00.bak.json
--
-- Verwendet FilePath-Funktionen:
-- - takeDirectory: "folder/data.json" -> "folder"
-- - takeBaseName:  "folder/data.json" -> "data"
-- - takeExtension: "folder/data.json" -> ".json"
-- - (</>): Verbindet Pfade korrekt für jedes OS
makeBackup :: FilePath -> [Record] -> IO ()
makeBackup path records = do
    let dir  = takeDirectory path   -- Verzeichnis extrahieren
    let base = takeBaseName path    -- Dateiname ohne Endung
    let ext  = takeExtension path   -- Dateiendung (.json)

    -- Aktuellen Zeitstempel formatieren
    timestamp <- formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> getCurrentTime

    -- Backup-Pfad zusammenbauen
    let backupFile = dir </> (base ++ "_" ++ timestamp ++ ".bak" ++ ext)

    B.writeFile backupFile (encodePretty records)
    putStrLn $ blue ++ "Backup erstellt: " ++ backupFile ++ reset
