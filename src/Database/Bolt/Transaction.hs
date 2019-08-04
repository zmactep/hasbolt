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
                                                , run
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

-- |Runs a list of 'BoltActionT' as an atomic operation and returns all the records or throws 'TxError' if something went wrong
transact :: [BoltActionT IO [Record]] -> BoltActionT IO [Record]
transact actions = do
  pipe <- ask
  liftIO $ txBegin pipe
  liftIO $ catch
    (join <$> traverse (run pipe) actions <* void (txCommit pipe))
    (handler pipe)
 where
  handler :: Pipe -> IOException -> IO [Record]
  handler pipe _ = txRollback pipe >> throwM TxError

-- |Runs a list of 'BoltActionT' as an atomic operation and discards all outputs or throws 'TxError' if something went wrong
transact_ :: [BoltActionT IO [Record]] -> BoltActionT IO ()
transact_ = void . transact

txCommand :: Text -> Pipe -> IO Response
txCommand cmd pipe = do
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
