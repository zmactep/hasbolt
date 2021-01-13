{-# LANGUAGE RecordWildCards #-}
module Database.Bolt.Connection.Connection where

import Control.Applicative           (pure, (<$>))
import Control.Exception             (throwIO)
import Control.Monad                 (forM_, when)
import Control.Monad.Trans           (MonadIO (..))
import Data.ByteString               (ByteString, null)
import Data.Default                  (Default (..))
import Database.Bolt.Connection.Type (BConnection (..), BoltError (..))
import GHC.Stack                     (HasCallStack, withFrozenCallStack)
import Network.Connection            (ConnectionParams (..), connectTo, connectionClose,
                                      connectionGetExact, connectionPut, connectionSetSecure,
                                      initConnectionContext)
import Network.Socket                (PortNumber)
import Prelude                       hiding (null)
import System.Timeout                (timeout)

connect :: MonadIO m => HasCallStack => Bool -> String -> PortNumber -> Int -> m BConnection
connect secure host port time = liftIO $ do
    ctx  <- initConnectionContext
    conn <- timeoutThrow time $
      connectTo ctx ConnectionParams
        { connectionHostname  = host
        , connectionPort      = port
        , connectionUseSecure = Nothing
        , connectionUseSocks  = Nothing
        }
    when secure $ connectionSetSecure ctx conn def
    pure $ BConnection conn time

close :: MonadIO m => HasCallStack => BConnection -> m ()
close BConnection{..} = liftIO $ timeoutThrow bcTimeout $ connectionClose bcConn

recv :: MonadIO m => HasCallStack => BConnection -> Int ->  m (Maybe ByteString)
recv BConnection{..} = liftIO . (filterMaybe (not . null) <$>) . timeoutThrow bcTimeout . connectionGetExact bcConn
  where
    filterMaybe :: (a -> Bool) -> a -> Maybe a
    filterMaybe p x | p x       = Just x
                    | otherwise = Nothing

send :: MonadIO m => HasCallStack => BConnection -> ByteString -> m ()
send BConnection{..} = liftIO . timeoutThrow bcTimeout . connectionPut bcConn

sendMany :: MonadIO m => HasCallStack => BConnection -> [ByteString] -> m ()
sendMany conn@BConnection{..} chunks = liftIO $ forM_ chunks $ timeoutThrow bcTimeout . send conn

timeoutThrow :: HasCallStack => Int -> IO a -> IO a
timeoutThrow time action = withFrozenCallStack $ do
  -- 'timeout' wants microseconds
  res <- timeout (1000000 * time) action
  case res of
    Just a  -> return a
    Nothing -> throwIO TimeOut
