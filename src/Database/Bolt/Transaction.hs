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
                                                , pullKeys
                                                , pullRecords
                                                )
import           Database.Bolt.Record

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
import           Data.Map.Strict                ( empty )
import           Data.Text                      ( Text )

data TxError = TxError deriving Show
instance Exception TxError

-- |Runs a list of cypher queries with parameters as an atomic operation and returns all the records
transact :: MonadIO m => [Cypher] -> BoltActionT m [Record]
transact cyphers = do
  pipe <- ask
  liftIO $ txBegin pipe
  liftIO $ catch (join <$> traverse (sendCypher pipe) cyphers <* void (txCommit pipe))
                 (\TxError -> txRollback pipe >> throwM TxError)
 where
  handler :: IOException -> IO [Record]
  handler e = print e >> throwM TxError
  sendCypher :: Pipe -> Cypher -> IO [Record]
  sendCypher pipe cypher = do
    putStrLn $ "Sending Cypher " ++ show (cypherQuery cypher)
    catch (pullKeys pipe cypher >>= pullRecords True pipe) handler

-- |Runs a list of cypher queries with parameters as an atomic operation and discards all outputs
transact_ :: MonadIO m => [Cypher] -> BoltActionT m ()
transact_ = void . transact

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
