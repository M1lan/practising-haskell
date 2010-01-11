-- basic I/O
main = do
  putStrLn "Greetings! Who are you?"
  inpStr <- getLine
  putStrLn $ "Welcome, " ++ inpStr ++ "!"
