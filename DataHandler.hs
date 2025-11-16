-- Modul zum Laden und Speichern von JSON-Dateien
module DataHandler
  ( loadRecords
  , saveRecords
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import Record (Record)

-- JSON-Datei einlesen → Liste von Records zurückgeben
-- Falls Datei nicht existiert → leere Liste
loadRecords :: FilePath -> IO [Record]
loadRecords path = do
  maybeList <- decodeFileStrict path      -- JSON einlesen
  return (maybe [] id maybeList)          -- Nothing → []

-- Liste von Records wieder als JSON speichern
saveRecords :: FilePath -> [Record] -> IO ()
saveRecords = encodeFile