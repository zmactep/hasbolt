{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import System.IO
import Database.Bolt
import Data.Text as T

upload :: IO ()
upload = do
  handle <- openFile "/Users/pavel/Dropbox/Drive/DEV/playground/neo4j.init.txt" ReadMode
  !contents <- T.pack <$> hGetContents handle
  pipe <- connect $ def { host="127.0.0.1", user="neo4j", password="12345" }
  _ <- run pipe $ query "MATCH (n)-[r]-() DELETE n,r"
  _ <- run pipe $ query "MATCH (n) DELETE n"
  _ <- run pipe $ query contents
  close pipe
  hClose handle

download :: IO ()
download = do
  pipe <- connect $ def { host="192.168.40.102", user="neo4j", password="lvbm123" }
  results <- run pipe $ query "MATCH (m:Structure {chain: \"H\"}) RETURN m LIMIT 4000"
  forM_ results $ \result -> do
    node <- result `at` "m" >>= exact :: IO Node
    family <- (nodeProps node) `at` "family" >>= exact :: IO Text
    print family
  close pipe

main :: IO ()
main = download
