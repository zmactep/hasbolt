{-# LANGUAGE OverloadedStrings #-}

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import           Database.Bolt

cypher :: Text
cypher = "MATCH (n) WHERE {label} IN labels(n) RETURN collect(ID(n)) AS ids"

params :: Map Text Value
params = M.fromList [("label", T "AbChain")]

singleRequest :: BoltActionT IO [Int]
singleRequest = head <$> queryP cypher params >>= (`at` "ids") >>= exact

doubleRequest :: BoltActionT IO ([Int], [Int])
doubleRequest = do x <- singleRequest
                   y <- singleRequest
                   pure (x, y)

singleRequest' :: BoltActionT IO [Int]
singleRequest' = head <$> queryP' cypher params >>= (`at` "ids") >>= exact

doubleRequest' :: BoltActionT IO ([Int], [Int])
doubleRequest' = do x <- singleRequest'
                    y <- singleRequest'
                    pure (x, y)

mainS :: IO ()
mainS = do pipe <- connect $ def { user = "neo4j", password = "lvbm123", host = "192.168.40.166" }
           result <- run pipe $ singleRequest
           print result
           close pipe

mainD :: IO ()
mainD = do pipe <- connect $ def { user = "neo4j", password = "lvbm123", host = "192.168.40.166" }
           result <- run pipe $ doubleRequest
           print result
           close pipe

mainS' :: IO ()
mainS' = do pipe <- connect $ def { user = "neo4j", password = "lvbm123", host = "192.168.40.166" }
            result <- run pipe $ singleRequest'
            print result
            close pipe

mainD' :: IO ()
mainD' = do pipe <- connect $ def { user = "neo4j", password = "lvbm123", host = "192.168.40.166" }
            result <- run pipe $ doubleRequest'
            print result
            close pipe
