module Database.Bolt
    ( BoltActionT
    , BoltError (..), UnpackError (..)
    , connect, close, reset
    , run, runE, queryP, query, queryP_, query_
    , transact
    , (=:), props
    , Pipe
    , BoltCfg (..)
    , Value (..), IsValue (..), Structure (..), Record, RecordValue (..), exact, exactMaybe, at
    , maybeAt, Node (..), Relationship (..), URelationship (..), Path (..)
    ) where

import           Database.Bolt.Connection hiding (query, queryP)
import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Type
import           Database.Bolt.Record
import           Database.Bolt.Transaction
import           Database.Bolt.Value.Instances ()
import           Database.Bolt.Value.Type

import           Data.Text                     (Text)
import           Data.Map.Strict               (Map)
import           Control.Monad.Trans           (MonadIO)

-- |Runs Cypher query and returns list of obtained 'Record's. Strict version
query :: MonadIO m => Text -> BoltActionT m [Record]
query = query'

-- |Runs Cypher query with parameters and returns list of obtained 'Record's. Strict version
queryP :: MonadIO m => Text -> Map Text Value -> BoltActionT m [Record]
queryP = queryP'
