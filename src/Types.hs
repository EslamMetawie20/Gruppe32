{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON, FromJSON)

-- Datentyp für einen einzelnen Eintrag in der JSON-Datei
data Record = Record
  { id    :: Int
  , name  :: String
  , value :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON Record
instance ToJSON Record

-- Repräsentiert alle möglichen Befehle der Anwendung
data Command
  = Insert Int String Double
  | Update Int String Double
  | Delete Int
  | Filter Double
  | Query String
  | Stats
  | List
  | Save (Maybe FilePath)
  | Print
  | Help
  | Version
  | Quit
  | Unknown String
  deriving (Show, Eq)

-- Eigener Datentyp für Ergebnisse (Erfolg oder Fehler)
data Result a = Success a | Failure String deriving (Show, Eq)