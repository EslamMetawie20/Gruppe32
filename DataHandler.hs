{-# LANGUAGE OverloadedStrings #-} -- AESONs converter

-- Modul für JSON-Verarbeitung
module DataHandler
  ( Record(..)
  , loadRecords
  ) where

-- https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, object, (.=), (.:), decodeFileStrict)
import Data.Aeson.Types (Value(Object))


-- Benutzerdefinierter Datentyp mit Record-Syntax
data Record = Record
  { recordId :: Int,      -- eindeutige ID
    name     :: String,   -- Name des Eintrags
    value    :: Int       -- numerischer Wert
  } deriving (Show, Eq)

-- Zeigt Aeson, wie Record in JSON umgewandelt wird
instance ToJSON Record where
  toJSON record = object
    [ 
      "id"    .= recordId record, -- .= sagt Aeson, dass Feld "id" den Wert recordId hat
      "name"  .= name record, 
      "value" .= value record
    ]

-- Zeigt Aeson, wie JSON in Record umgewandelt wird
instance FromJSON Record where
  parseJSON (Object v) = do
    recordsID     <- v .: "id"            -- Lies Felder
    name          <- v .: "name"          -- vvvvvvvvvvv
    value         <- v .: "value"         -- same...
    return (Record recordsID name value)  -- Erstelle Record
  parseJSON _ = fail "Erwartet ein JSON-Object"

-- Lädt Records aus einer JSON-Datei
-- Gibt entweder eine Liste von Records zurück oder eine Fehlermeldung
loadRecords :: FilePath -> IO (Either String [Record])
loadRecords filepath = do
  -- Lese und parse die JSON-Datei direkt
  result <- decodeFileStrict filepath
  
  -- Wandle Maybe [Record] in Either String [Record] um
  case result of
    Just records -> return (Right records)  -- Erfolg: Records da
    Nothing      -> return (Left "Fehler: Konnte JSON nicht parsen")  -- Fehler
