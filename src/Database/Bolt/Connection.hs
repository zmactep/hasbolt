module Database.Bolt.Connection where

import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Type
import           Database.Bolt.Record

import           Control.Monad.IO.Class        (MonadIO (..), liftIO)
import           Control.Monad.Trans.Reader    (ReaderT (..), ask, runReaderT)
import           Data.Text                     (Text)
import           Data.Map.Strict               (Map, empty)

import           System.IO.Unsafe              (unsafeInterleaveIO)

-- |Monad Transformer to do all BOLT actions in
type BoltActionT = ReaderT Pipe

-- |Runs BOLT action on selected pipe
run :: Pipe -> BoltActionT m a -> m a
run = flip runReaderT

-- |Runs Cypher query with parameters and returns list of obtained 'Record's
queryP :: MonadIO m => Text -> Map Text Value -> BoltActionT m [Record]
queryP cypher params = do keys <- pullKeys
                          pipe <- ask
                          liftIO $ pullRecords pipe keys
  where
        pullKeys :: MonadIO m => BoltActionT m [Text]
        pullKeys =  sendRequest cypher params $ \status -> do
                      pipe <- ask
                      flush pipe RequestPullAll
                      mkKeys status
    
        pullRecords :: Pipe -> [Text] -> IO [Record]
        pullRecords pipe keys = do resp <- fetch pipe
                                   if isSuccess resp
                                     then return []
                                     else do let record = mkRecord keys resp
                                             rest <- unsafeInterleaveIO $ pullRecords pipe keys
                                             return (record:rest)

-- |Runs Cypher query and returns list of obtained 'Record's
query :: MonadIO m => Text -> BoltActionT m [Record]
query cypher = queryP cypher empty

-- |Runs Cypher query with parameters and ignores response
queryP_ :: MonadIO m => Text -> Map Text Value -> BoltActionT m ()
queryP_ cypher params = sendRequest cypher params $ \_ -> do
                          pipe <- ask
                          discardAll pipe

-- |Runs Cypher query and ignores response
query_ :: MonadIO m => Text -> BoltActionT m ()
query_ cypher = queryP_ cypher empty

-- |Sends request to database and makes an action
sendRequest :: MonadIO m => Text -> Map Text Value -> (Response -> BoltActionT m a) -> BoltActionT m a
sendRequest cypher params f = do pipe <- ask
                                 flush pipe $ RequestRun cypher params
                                 status <- fetch pipe
                                 if isSuccess status then f status
                                                     else do ackFailure pipe
                                                             mkFailure status
