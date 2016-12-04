 {-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Bolt

main :: IO ()
main = do pipe <- connect $ def { user = "neo4j", password = "12345" }
          let doit = run pipe
          let myQuery = "MATCH (n:Person {name: \"Tom Hanks\"}) RETURN n,n.born"
          putStrLn $ "Answer on query " ++ show myQuery
          resps <- doit (query myQuery)
          print resps
          close pipe
