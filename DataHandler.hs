-- Modul zum Laden und Speichern von JSON-Dateien
module DataHandler
  ( loadRecords
  , saveRecords
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import Record (Record)
import Control.Exception (try, IOException)

-- Datei lesen → Liste von Records zurückgeben
-- Wenn Datei leer/fehlt/fehlerhaft → [] mit Fehlermeldung
loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    result <- try (decodeFileStrict path) :: IO (Either IOException (Maybe [Record]))
    case result of
        Left err -> do
            putStrLn ("Fehler beim Lesen der Datei: " ++ show err)
            return []
        Right Nothing -> do
            putStrLn "Fehler: JSON-Format ungültig oder Datei leer."
            return []
        Right (Just records) -> return records

-- Liste von Records als JSON speichern
saveRecords :: FilePath -> [Record] -> IO ()
saveRecords path records = do
    result <- try (encodeFile path records) :: IO (Either IOException ())
    case result of
        Left err -> putStrLn ("Fehler beim Speichern: " ++ show err)
        Right _  -> return ()