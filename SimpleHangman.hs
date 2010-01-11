-- file: SimpleHangman.hs
-- simple implementation of Hangman game. Following another tutorial here.
hangman :: IO ()
hangman =
    do putStrLn "Think of a word: "
       word <- sgetLine
       putStrLn "Try to guess it:"
       guess word

sgetLine :: IO String
sgetLine = do x <- getChar
              if x == '\n' then
                  do putChar x
                     return []
               else
                   do putChar '-'
                      xs <- sgetLine
                      return (x:xs)


guess :: String -> IO ()
guess word =  
    do putStr "> "
       xs <- getLine
       if xs == word then
           putStrLn "YESSSSSss!111"
        else
            do putStrLn (diff word xs)
               guess word

-- guess word =
--     do { putStr "> "
--          xs <- getLine
--          if (p(xs))
--             terminate xs
--          else
--              do success xs
--                 guess word
--        }

-- guess word =
--     do { putStr msg
--          xs <- getLine
--          if (p(xs))
--             terminate xs
--          else
--              do success xs
--                 guess (next state)
--        }

-- guess word =
--     do { prelude
--          xs <- getLine
--          if (p(xs))
--             terminate xs
--          else
--              do success xs
--                 guess (next state)
--        }

-- guess pr p t s n w =
--     do pr :: IO()
--        xs <- getLine
--        if (p(xs))
--        t xs
--        else
--            do s xs
--               guess pr p t s n (n w)
                

diff :: String -> String -> String
diff xs ys =
    [if elem x ys then x else '-' | x <- xs]