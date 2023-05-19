import System.Directory
import Data.List

main :: IO ()
main = do
  putStr "Substring: "
  substring <- getLine
  if substring == ""
  then putStrLn "Cancelled"
  else do
    cs <- getDirectoryContents "."
    fps <- mapM_ (\x -> if isInfixOf substring x
                        then do
                          putStrLn $ "Removing file: " ++ x
                          removeFile x
                        else putStr ""
                 ) cs
    return ()