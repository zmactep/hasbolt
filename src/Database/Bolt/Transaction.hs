{-# LANGUAGE OverloadedStrings #-}
module Database.Bolt.Transaction
  ( transact
  , transactRead
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( ask )
import           Control.Monad.Trans            ( MonadIO(..) )
import           Control.Monad.Except           ( MonadError(..) )
import qualified Control.Monad.Catch            as MC (MonadCatch, onException)

import           Data.Map.Strict                ( Map, empty, fromList, union )
import           Data.Text                      ( Text )

import           Database.Bolt.Connection           ( BoltActionT
                                                    , query', sendRawRequest
                                                    )
import           Database.Bolt.Connection.Instances ( dbExtra, notifExtra )
import           Database.Bolt.Connection.Type      ( Request(..)
                                                    , pipe_version, pipeNotificationsMinimumSeverity, pipeNotificationsDisabledCategories
                                                    , pipeDatabase
                                                    )
import           Database.Bolt.Value.Helpers        ( isV3 )
import           Database.Bolt.Value.Type           ( Value, (=:) )

-- |Runs a sequence of actions as transaction. All queries would be rolled back
-- in case of any exception inside the block.
transact :: (MonadIO m, MC.MonadCatch m) => BoltActionT m a -> BoltActionT m a
transact actions = do
    txBegin empty
    result <- actions `MC.onException` txRollback
                      `catchError` \e -> txRollback >> throwError e
    txCommit
    pure result

-- |Runs a sequence of actions as a read transaction. Uses @mode: \"r\"@ in BEGIN
-- to route queries to read replicas in a cluster. All queries would be rolled back
-- in case of any exception inside the block.
transactRead :: (MonadIO m, MC.MonadCatch m) => BoltActionT m a -> BoltActionT m a
transactRead actions = do
    txBegin (fromList ["mode" =: ("r" :: Text)])
    result <- actions `MC.onException` txRollback
                      `catchError` \e -> txRollback >> throwError e
    txCommit
    pure result

txBegin :: MonadIO m => Map Text Value -> BoltActionT m ()
txBegin modeExtra = do
  pipe <- ask
  if isV3 $ pipe_version pipe
     then let nExtra = notifExtra (pipe_version pipe) (pipeNotificationsMinimumSeverity pipe) (pipeNotificationsDisabledCategories pipe)
          in void $ sendRawRequest $ RequestBegin (nExtra `union` dbExtra (pipeDatabase pipe) `union` modeExtra)
     else void $ query' "BEGIN"

txCommit :: MonadIO m => BoltActionT m ()
txCommit = do
  pipe <- ask
  if isV3 $ pipe_version pipe
     then void $ sendRawRequest RequestCommit
     else void $ query' "COMMIT"

txRollback :: MonadIO m => BoltActionT m ()
txRollback = do
  pipe <- ask
  if isV3 $ pipe_version pipe
     then void $ sendRawRequest RequestRollback
     else void $ query' "ROLLBACK"
