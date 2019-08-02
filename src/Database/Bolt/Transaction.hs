{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Database.Bolt.Transaction
  ( transact
  )
where

import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Type
import           Database.Bolt.Connection       ( BoltActionT
                                                , sendRequest
                                                )

import           Control.Monad.Catch            ( Exception
                                                , catch
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..)
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ask )
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.Map.Strict                ( empty )
import           Data.Text                      ( Text )

data TxError = TxError deriving Show
instance Exception TxError

-- |Runs a list of cypher queries with parameters as an atomic operation
transact :: MonadIO m => [Cypher] -> BoltActionT m ()
transact cyphers = do
  pipe <- ask
  liftIO (txBegin pipe) >>= \case
    ResponseSuccess _ -> liftIO $ catch
      (do
        traverse_ (sendCypher pipe) cyphers
        void $ txCommit pipe
      )
      (\TxError -> txRollback pipe)
    _ -> ackFailure pipe
 where
  sendCypher :: Pipe -> Cypher -> IO ()
  sendCypher pipe cypher = sendRequest pipe cypher >>= \case
    ResponseFailure _ -> throwM TxError
    _                 -> pure () --TODO: Fetch records

txCommand :: Text -> Pipe -> IO Response
txCommand cmd pipe = do
  flush pipe $ RequestRun cmd empty
  fetch pipe

txCommit :: Pipe -> IO Response
txCommit = txCommand "COMMIT"

txBegin :: Pipe -> IO Response
txBegin = txCommand "BEGIN"

txRollback :: Pipe -> IO ()
txRollback pipe = do
  flush pipe RequestAckFailure
  void $ fetch pipe
  void $ txCommand "ROLLBACK" pipe
