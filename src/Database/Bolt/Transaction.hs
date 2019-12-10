{-# LANGUAGE OverloadedStrings #-}
module Database.Bolt.Transaction
  ( transact
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Except           ( MonadError(..) )

import           Database.Bolt.Connection       ( BoltActionT
                                                , query'
                                                )

-- |Runs a sequence of actions as transaction. All queries would be rolled back
-- in case of any exception inside the block.
transact :: MonadIO m => BoltActionT m a -> BoltActionT m a
transact actions = do
    txBegin
    let processErrors = flip catchError $ \e -> txRollback >> throwError e
    result <- processErrors actions
    txCommit
    pure result

txBegin :: MonadIO m => BoltActionT m ()
txBegin = void $ query' "BEGIN"

txCommit :: MonadIO m => BoltActionT m ()
txCommit = void $ query' "COMMIT"

txRollback :: MonadIO m => BoltActionT m ()
txRollback = void $ query' "ROLLBACK"
