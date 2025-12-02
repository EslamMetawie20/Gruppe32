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
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Eigener Datentyp für Ergebnisse (Erfolg oder Fehler)
data Result a = Success a | Failure String deriving (Show, Eq)