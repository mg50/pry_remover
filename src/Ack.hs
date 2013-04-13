module Ack where

import System.IO
import System.Process (readProcess)
import Parse
import Control.Monad

search :: IO [PryInstance]
search = do ackOutput <- readProcess "ack" ["\"binding\\.pry|binding\\.remote_pry\""] []
            return $ getPryInstances ackOutput


promptDelete :: [String] -> Int -> IO Bool
promptDelete lines lineNum = do putStr $ (show lineNum) ++ ": "
                                putStrLn $ lines !! lineNum
                                putStrLn "Delete? y/n"
                                resp <- getLine
                                putStrLn "\n"
                                return $ resp == "y"

removeLines :: Handle -> [String] -> [Int] -> IO ()
removeLines hdl lines deletions =
  let newLines = [l | (l, idx) <- zip lines [0..], not (idx `elem` deletions)]
  in hPutStr hdl (unlines newLines)

removePryInstance :: PryInstance -> IO ()
removePryInstance (PryInstance fileName lineNums) =
  withFile fileName ReadWriteMode $ \hdl ->
    do lines <- liftM lines $ hGetContents hdl
       putStrLn fileName
       deletions <- filterM (promptDelete lines) lineNums
       removeLines hdl lines deletions
