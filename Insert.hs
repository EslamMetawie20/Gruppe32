import System.Exit (exitFailure)

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "JSON-CLI Tool" )

runCommand :: Command -> IO ()
runCommand (Insert filePath newId newName newValue) = do
  maybeRecords <- decodeFileStrict filePath :: IO (Maybe [Record])
  let records = maybe [] id maybeRecords  -- Leere Liste, falls Datei nicht existiert
  let newRecord = Record newId newName newValue
  let updatedRecords = records ++ [newRecord]
  encodeFile filePath updatedRecords
  putStrLn $ "Record eingefügt: " ++ show newRecord
