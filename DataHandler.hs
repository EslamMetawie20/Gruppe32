-- Modul zum Laden und Speichern von JSON-Dateien
module DataHandler
  ( loadRecords
  , saveRecords
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import Record (Record)

-- Datei lesen → Liste von Records zurückgeben
-- Wenn Datei leer/fehlt → []
loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    result <- decodeFileStrict path
    return (maybe [] id result)

-- Liste von Records als JSON speichern
saveRecords :: FilePath -> [Record] -> IO ()
saveRecords = encodeFile