-- Diese Spracherweiterungen erlauben automatische Ableitung von Typklassen
-- DeriveGeneric: Ermöglicht 'deriving (Generic)' für automatische JSON-Konvertierung
-- DeriveAnyClass: Erlaubt das Ableiten beliebiger Typklassen (hier: ToJSON, FromJSON)
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Types.hs - Zentrale Datentyp-Definitionen
-- Dieses Modul definiert alle Datenstrukturen, die im Projekt verwendet werden.
-- Andere Module importieren diese Typen von hier.
module Types 
  ( Record(..)
  , Command(..)
  -- ANSI-Farbcodes (zentral definiert)
  , red, green, blue, bold, reset
  ) where

import GHC.Generics (Generic)   -- Für automatische JSON-Ableitung
import Data.Aeson   (ToJSON, FromJSON)  -- JSON-Bibliothek

-- | Record: Ein einzelner Datensatz in unserer JSON-Datei
data Record = Record
  { id    :: Int       -- Eindeutige ID des Eintrags
  , name  :: String    -- Name/Bezeichnung
  , value :: Double    -- Numerischer Wert
  } deriving (Show,    -- Ermöglicht: show record -> "Record {id = 1, ...}"
              Eq,      -- Ermöglicht Vergleiche: record1 == record2
              Generic) -- Ermöglicht automatische JSON-Konvertierung

-- Diese Instanzen nutzen Generic, um automatisch JSON zu erzeugen/parsen
-- Ohne Generic müssten wir das manuell implementieren!
instance FromJSON Record  -- JSON-String -> Record (beim Laden)
instance ToJSON Record    -- Record -> JSON-String (beim Speichern)

-- | Command: Alle möglichen Befehle der Anwendung
-- Dass ist ein "Summentyp"
-- Ein Command kann NUR EINER dieser Varianten sein.
data Command
  = Insert Int String Double  -- Insert ID Name Wert
  | Update Int String Double  -- Update ID NeuerName NeuerWert
  | Delete Int                -- Delete ID
  | Filter Double             -- Filter Schwellwert (zeigt alle > Wert)
  | Query String              -- Query Suchbegriff (sucht im Namen)
  | Stats                     -- Statistik anzeigen
  | List                      -- Alle Einträge auflisten
  | Save (Maybe FilePath)     -- Speichern (Maybe = optional mit Dateiname)
  | Print                     -- Ausgabe auf Konsole
  | Help                      -- Hilfe anzeigen
  | Version                   -- Version anzeigen
  | Quit                      -- Programm beenden
  | Unknown String            -- Unbekannter Befehl (für Fehlermeldung)
  deriving (Show, Eq)

-- | ANSI-Escape-Codes für farbige Terminal-Ausgabe
-- Verwendung: putStrLn (red ++ "Fehler!" ++ reset)
red, green, blue, bold, reset :: String
red   = "\ESC[31m"   -- Rot (für Fehler)
green = "\ESC[32m"   -- Grün (für Erfolg)
blue  = "\ESC[34m"   -- Blau (für Info)
bold  = "\ESC[1m"    -- Fett
reset = "\ESC[0m"    -- Zurücksetzen auf Standard