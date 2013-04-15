module Main where

import System.IO
import System.Process
import Control.Monad
import Parse (PryInstance(..), getPryInstances)

-- ack is broken and tries to read from stdin when using readCommand, so set up everything manually
search :: IO [PryInstance]
search = do print "starting"
            let cmd = ShellCommand "ack \"binding.pry|binding.remote_pry\""
            let process = CreateProcess cmd Nothing Nothing Inherit CreatePipe Inherit True False
            (_, (Just hdl), _, pid) <- createProcess process
            waitForProcess pid
            ackOutput <- hGetContents hdl
            print ackOutput
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
  let remainingLines = [line | (line, idx) <- zip lines [0..], not (idx `elem` deletions)]
  in hPutStr hdl (unlines remainingLines)

removePryInstance :: PryInstance -> IO ()
removePryInstance (PryInstance fileName lineNums) =
  withFile fileName ReadWriteMode $ \hdl ->
    do fileLines <- liftM lines $ hGetContents hdl
       putStrLn fileName
       deletions <- filterM (promptDelete fileLines) lineNums
       removeLines hdl fileLines deletions

main :: IO ()
main = search >>= mapM_ removePryInstance
