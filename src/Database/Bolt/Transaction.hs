{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transaction
  ( TxError
  , transact
  , transact_
  )
where

import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Type
import           Database.Bolt.Connection       ( BoltActionT
                                                , queryP_
                                                , queryP'
                                                , run
                                                )
import           Database.Bolt.Record
import           Database.Bolt.Value.Type

import           Control.Exception              ( IOException )
import           Control.Monad                  ( join )
import           Control.Monad.Catch            ( Exception
                                                , catch
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..)
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ask )
import           Data.Functor                   ( void )
import           Data.Map                       ( Map )
import           Data.Map.Strict                ( empty )
import           Data.Text                      ( Text )

data TxError = TxError deriving Show
instance Exception TxError

-- |Runs a list of cypher queries with parameters as an atomic operation and returns all the records
transact :: MonadIO m => [Cypher] -> BoltActionT m [Record]
transact = transaction False

-- |Runs a list of cypher queries with parameters as an atomic operation and discards all outputs
transact_ :: MonadIO m => [Cypher] -> BoltActionT m ()
transact_ = void . transaction True

transaction :: MonadIO m => Bool -> [Cypher] -> BoltActionT m [Record]
transaction discard cyphers = do
  pipe <- ask
  liftIO $ txBegin pipe
  liftIO $ catch
    (join <$> traverse (sendCypher pipe) cyphers <* void (txCommit pipe))
    (\TxError -> txRollback pipe >> throwM TxError)
 where
  handler :: IOException -> IO [Record]
  handler e = print e >> throwM TxError
  txQuery :: Text -> Map Text Value -> BoltActionT IO [Record]
  txQuery q p = if discard then [] <$ queryP_ q p else queryP' q p
  sendCypher :: Pipe -> Cypher -> IO [Record]
  sendCypher pipe c = do
    putStrLn $ "Sending Cypher " ++ show (cypherQuery c)
    catch (run pipe $ txQuery (cypherQuery c) (cypherParams c)) handler

txCommand :: Text -> Pipe -> IO Response
txCommand cmd pipe = do
  putStrLn $ "Sending command " ++ show cmd
  flush pipe $ RequestRun cmd empty
  void $ fetch pipe
  flush pipe RequestPullAll
  fetch pipe

txBegin :: Pipe -> IO ()
txBegin = void . txCommand "BEGIN"

txCommit :: Pipe -> IO ()
txCommit = void . txCommand "COMMIT"

txRollback :: Pipe -> IO ()
txRollback = void . txCommand "ROLLBACK"
