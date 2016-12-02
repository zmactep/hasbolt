 {-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.Bolt

main :: IO ()
main = do pipe <- connect $ def { user = "neo4j", password = "12345" }
          putStrLn "It is alive!"
          close pipe
