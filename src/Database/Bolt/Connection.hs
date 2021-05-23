{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Connection
  ( BoltActionT
  , BoltError (..)
  , UnpackError (..)
  , at
  , run, runE
  , queryP, query
  , queryP', query'
  , queryP_, query_
  ) where

import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type
import           Database.Bolt.Record

import           Control.Exception             (throwIO)
import           Control.Monad                 (void)
import           Control.Monad.Except          (MonadError (..), runExceptT)
import           Control.Monad.Reader          (MonadReader (..), runReaderT)
import           Control.Monad.Trans           (MonadIO (..))
import           Data.Map.Strict               (Map, empty, fromList)
import           Data.Text                     (Text)
import           GHC.Stack                     (HasCallStack)

import           System.IO.Unsafe              (unsafeInterleaveIO)

-- |Runs BOLT action on selected pipe
runE :: MonadIO m => HasCallStack => Pipe -> BoltActionT m a -> m (Either BoltError a)
runE pipe action = runExceptT (runReaderT (runBoltActionT action) pipe)

-- |Runs BOLT action on selected pipe (with errors throw)
run :: MonadIO m => HasCallStack => Pipe -> BoltActionT m a -> m a
run pipe action = do result <- runE pipe action
                     case result of
                       Right x -> pure x
                       Left r  -> liftIO $ throwIO r

-- |Runs Cypher query with parameters and returns list of obtained 'Record's. Lazy version
queryP :: MonadIO m => HasCallStack => Text -> Map Text Value -> BoltActionT m [Record]
queryP = querySL False

-- |Runs Cypher query and returns list of obtained 'Record's. Lazy version
query :: MonadIO m => HasCallStack => Text -> BoltActionT m [Record]
query cypher = queryP cypher empty

-- |Runs Cypher query with parameters and returns list of obtained 'Record's. Strict version
queryP' :: MonadIO m => HasCallStack => Text -> Map Text Value -> BoltActionT m [Record]
queryP' = querySL True

-- |Runs Cypher query and returns list of obtained 'Record's. Strict version
query' :: MonadIO m => HasCallStack => Text -> BoltActionT m [Record]
query' cypher = queryP' cypher empty

-- |Runs Cypher query with parameters and ignores response
queryP_ :: MonadIO m => HasCallStack => Text -> Map Text Value -> BoltActionT m ()
queryP_ cypher params = do void $ sendRequest cypher params empty
                           ask >>= liftE . discardAll

-- |Runs Cypher query and ignores response
query_ :: MonadIO m => HasCallStack => Text -> BoltActionT m ()
query_ cypher = queryP_ cypher empty

-- Helper functions

querySL :: MonadIO m => HasCallStack => Bool -> Text -> Map Text Value -> BoltActionT m [Record]
querySL strict cypher params = do keys <- pullKeys cypher params empty
                                  pullRecords strict keys

pullKeys :: MonadIO m => HasCallStack => Text -> Map Text Value -> Map Text Value -> BoltActionT m [Text]
pullKeys cypher params ext = do pipe <- ask
                                status <- sendRequest cypher params ext
                                liftE $ flush pipe RequestPullAll
                                mkKeys status
  where
    mkKeys :: MonadIO m => Response -> BoltActionT m [Text]
    mkKeys (ResponseSuccess response) = response `at` "fields" `catchError` \(RecordHasNoKey _) -> pure []
    mkKeys x                          = throwError $ ResponseError (mkFailure x)

pullRecords :: MonadIO m => HasCallStack => Bool -> [Text] -> BoltActionT m [Record]
pullRecords strict keys = do pipe <- ask
                             resp <- liftE $ fetch pipe
                             cases resp
  where
    cases :: MonadIO m => Response -> BoltActionT m [Record]
    cases resp | isSuccess resp = pure []
               | isFailure resp = do ask >>= processError
                                     throwError $ ResponseError (mkFailure resp)
               | otherwise      = parseRecord resp

    parseRecord :: MonadIO m => Response -> BoltActionT m [Record]
    parseRecord resp = do
        pipe <- ask
        let record = fromList . zip keys $ recsList resp
        let pull = run pipe (pullRecords strict keys)
        rest <- liftIO $ if strict then pull
                                   else unsafeInterleaveIO pull
        pure (record:rest)

-- |Sends request to database and makes an action
sendRequest :: MonadIO m => HasCallStack => Text -> Map Text Value -> Map Text Value -> BoltActionT m Response
sendRequest cypher params ext =
  do pipe <- ask
     liftE $ do
         if isNewVersion (pipe_version pipe)
            then flush pipe $ RequestRunV3 cypher params ext
            else flush pipe $ RequestRun cypher params
         status <- fetch pipe
         if isSuccess status
           then pure status
           else do processError pipe
                   throwError $ ResponseError (mkFailure status)
