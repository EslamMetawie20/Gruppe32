-- Modul zur Verarbeitung der CLI-Befehle
module CLI (handleCommand) where

-- Diese Funktion empfängt den Befehl und seine Argumente
handleCommand :: String -> [String] -> IO ()
handleCommand cmd args = do
  putStrLn $ "Empfangener Befehl: " ++ cmd
  putStrLn $ "Argumente: " ++ show args
