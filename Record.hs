-- Definition eines Records
data Record = Record
  { id    :: Int      -- Feld 'id' vom Typ Int
  , name  :: String   -- Feld 'name' vom Typ String
  , value :: Double   -- Feld 'value' vom Typ Double
  } deriving (Show, Eq)
