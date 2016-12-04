 {-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Bolt

import Control.Monad ((>=>))

main :: IO ()
main = do pipe <- connect $ def { user = "neo4j", password = "12345" }
          let doit = run pipe
          let myQuery = "MATCH (n:Person {name: \"Tom Hanks\"}) RETURN n.born"
          putStrLn $ "Ignoring results for query: " ++ show myQuery
          doit (query_ myQuery)
          let myQueryWrong = "Some unparseable query to make Neo4j mad"
          putStrLn $ "Answer on query: " ++ show myQueryWrong
          resps <- doit (query myQueryWrong)
          _ <- traverse print resps
          let myQuery = "MATCH (n:Person) WHERE n.name CONTAINS \"Tom\" RETURN n"
          putStrLn $ "Answer on query: " ++ show myQuery
          resps <- doit (query myQuery)
          _ <- traverse (toNode >=> print) resps
          close pipe

toNode :: Monad m => Record -> m Node
toNode rec = rec `at` "n" >>= exact
