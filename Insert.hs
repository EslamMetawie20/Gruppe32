module Insert (handleInsert) where

import System.IO
import System.Directory (doesFileExist)

-- Datentyp Record
data Record = Record
  { rId    :: Int
  , rName  :: String
  , rValue :: Double
  } deriving (Show, Eq)

-- Hilfsfunktion: JSON-ähnliche Darstellung
recordToJSON :: Record -> String
recordToJSON (Record i n v) =
  "{\"id\":" ++ show i ++
  ",\"name\":\"" ++ n ++
  "\",\"value\":" ++ show v ++ "}"

-- Hilfsfunktion: Minimaler JSON-Parser (liest Liste von Records)
parseRecords :: String -> [Record]
parseRecords content =
  let entries = filter (not . null) (splitOn "}," content)
  in map parseRecord entries

-- Einzelnen Datensatz aus JSON-Zeile parsen
parseRecord :: String -> Record
parseRecord str =
  let getField key = between (key ++ "\":") "," str
      getStringField key = between (key ++ "\":\"") "\"" str
      i = read (getField "id") :: Int
      n = getStringField "name"
      v = read (between "value\":" "}" str) :: Double
  in Record i n v

-- Einfache String-Helferfunktionen
between :: String -> String -> String -> String
between start end s =
  case dropWhile (/= head start) s of
    [] -> ""
    xs -> takeWhile (/= head end) (drop (length start) xs)

splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  let (first, rest) = breakOn delim str
  in first : case rest of
       [] -> []
       x  -> splitOn delim (drop (length delim) x)

breakOn :: String -> String -> (String, String)
breakOn delim s =
  case span (/= head delim) s of
    (before, []) -> (before, [])
    (before, rest) ->
      if take (length delim) rest == delim
         then (before, drop (length delim) rest)
         else let (b2, r2) = breakOn delim (tail rest)
              in (before ++ [head rest] ++ b2, r2)

-- ------------------------------------------
-- Hauptlogik für Insert
-- ------------------------------------------

handleInsert :: [String] -> IO ()
handleInsert (file:idStr:name:valueStr:_) = do
  exists <- doesFileExist file
  contents <- if exists then readFile file else pure "[]"

  let records = if contents == "[]" || null contents then [] else parseRecord
