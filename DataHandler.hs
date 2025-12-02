-- Modul zum Laden und Speichern von JSON-Dateien
module DataHandler
  ( loadRecords
  , saveRecords
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty (encodePretty)

import Data.Aeson (decodeFileStrict, encodeFile)
import Record (Record)

import Control.Exception (try, IOException)
import System.Exit (exitFailure)

-- Datei lesen → Liste von Records zurückgeben
-- Wenn Datei leer/fehlt/fehlerhaft → ExitFailure mit Fehlermeldung
loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    result <- try (decodeFileStrict path) :: IO (Either IOException (Maybe [Record]))
    case result of
        Left err -> do
            putStrLn ("Fehler beim Lesen der Datei: " ++ show err)
            exitFailure
        Right Nothing -> do
            putStrLn "Fehler: JSON-Format ungültig oder Datei leer."
            exitFailure
        Right (Just records) -> return records

-- Liste von Records als JSON speichern
-- Wenn Datei fehlerhaft → ExitFailure mit Fehlermeldung
saveRecords :: FilePath -> [Record] -> IO ()
saveRecords path records = do
    result <- try (B.writeFile path (encodePretty records)) :: IO (Either IOException ())
    case result of
        Left err -> do
            putStrLn ("Fehler beim Speichern: " ++ show err)
            exitFailure
        Right _  -> return ()