module Main where

import System.IO (Handle)
import Control.Monad

removeIndex :: [a] -> Int -> [a]
removeIndex as idx = go as 0
  where go [] i = []
        go (a:as) i = if i == idx
                         then go as $! i + 1
                         else a:(go as $! i + 1)


removeLine :: Handle -> Int -> IO [String]
removeLine hdl lineNum = do lines <- liftM lines $ StrictIO.hGetContents hdl
                            return $ removeIndex lines lineNum



-- main = do pryInstances <- search
--           forM pryInstances print
