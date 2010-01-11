-- file tempfile.hs
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catch)
import Control.Exception(finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

{-blubb-}
myAction :: FilePath -> Handle -> IO()
myAction tempname temph =
    do -- display greeting
      putStrLn "welcome!"
      putStrLn $ "tempfile @ " ++ tempname
      
      -- initial position
      pos <- hTell temph
      putStrLn $ "initial position is: " ++ show pos
      
      -- write data to temp file
      let tempdata = show [1..10]
      putStrLn $ "writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
      hPutStrLn temph tempdata
      
      -- get new pos
      pos <- hTell temph
      putStrLn $ "After writing, my new pos is: " ++ show pos

      -- seek to start and display
      putStrLn $ "content: "
      hSeek temph AbsoluteSeek 0
      
      -- hGetContents lazy read
      c <- hGetContents temph
           
      -- Copy file to stdout
      putStrLn c

      -- literal
      putStrLn $ "literal: "
      print c


withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
      tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
      (tempfile, temph) <- openTempFile tempdir pattern

      finally (func tempfile temph)
              (do hClose temph
                  removeFile tempfile)