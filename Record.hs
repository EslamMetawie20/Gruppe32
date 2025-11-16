-- Definition eines Records
module Record where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON, FromJSON)

-- Ein Record besteht aus einer ID, einem Namen und einem numerischen Wert
data Record = Record
  { id    :: Int      -- Feld 'id' vom Typ Int
  , name  :: String   -- Feld 'name' vom Typ String
  , value :: Double   -- Feld 'value' vom Typ Double
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
