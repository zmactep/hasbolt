{-# LANGUAGE OverloadedStrings #-}
module Database.Bolt.Transaction
  ( transact
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( ask )
import           Control.Monad.Trans            ( MonadIO(..) )
import           Control.Monad.Except           ( MonadError(..) )

import           Database.Bolt.Connection       ( BoltActionT
                                                , query', sendRawRequest
                                                )
import           Database.Bolt.Connection.Type  ( Request(..)
                                                , pipe_version
                                                )
import           Database.Bolt.Value.Helpers    ( isNewVersion )
import Debug.Trace
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import GHC.IO (unsafePerformIO)

{-# NOINLINE txId #-}
txId :: IORef Int
txId = unsafePerformIO $ newIORef 0

-- |Runs a sequence of actions as transaction. All queries would be rolled back
-- in case of any exception inside the block.
transact :: MonadIO m => BoltActionT m a -> BoltActionT m a
transact actions = do
    newId <- fmap show $ liftIO $ atomicModifyIORef' txId $ \n -> (n + 1, n)
    liftIO $ putStrLn $ "before begin " <> newId
    txBegin
    liftIO $ putStrLn $ "after begin " <> newId
    let
      processErrors = flip catchError $ \e -> do
        liftIO $ putStrLn $ "before rollback " <> newId
        liftIO $ print ("EXCEPTION" :: String, e)
        txRollback
        liftIO $ putStrLn $ "after rollback " <> newId
        throwError e
    result <- processErrors actions
    liftIO $ putStrLn $ "before commit " <> newId
    txCommit
    liftIO $ putStrLn $ "after commit " <> newId
    pure result

txBegin :: MonadIO m => BoltActionT m ()
txBegin = do
  pipe <- ask
  if isNewVersion $ pipe_version pipe
     then void $ sendRawRequest $ RequestBegin mempty
     else void $ query' "BEGIN"

txCommit :: MonadIO m => BoltActionT m ()
txCommit = do
  pipe <- ask
  if isNewVersion $ pipe_version pipe
     then void $ sendRawRequest RequestCommit
     else void $ query' "COMMIT"

txRollback :: MonadIO m => BoltActionT m ()
txRollback = do
  pipe <- ask
  if isNewVersion $ pipe_version pipe
     then void $ sendRawRequest RequestRollback
     else void $ query' "ROLLBACK"
