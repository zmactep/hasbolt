{-# LANGUAGE RecordWildCards #-}
module Database.Bolt.Connection.Connection where

import Control.Applicative           (pure, (<$>))
import Control.Exception             (throwIO)
import Control.Monad                 (forM_, when)
import Control.Monad.Trans           (MonadIO (..))
import Data.ByteString               (ByteString, null)
import Data.Default                  (Default (..))
import Database.Bolt.Connection.Type (BoltError (..), ConnectionWithTimeout (..))
import GHC.Stack                     (HasCallStack, withFrozenCallStack)
import Network.Connection            (ConnectionParams (..), connectTo, connectionClose,
                                      connectionGetExact, connectionPut, connectionSetSecure,
                                      initConnectionContext)
import Network.Socket                (PortNumber)
import Prelude                       hiding (null)
import System.Timeout                (timeout)

connect
  :: MonadIO m
  => HasCallStack
  => Bool
     -- ^ Use secure connection
  -> String
     -- ^ Hostname
  -> PortNumber
  -> Int
     -- ^ Connection and read timeout in seconds
  -> m ConnectionWithTimeout
connect secure host port timeSec = liftIO $ do
    let timeUsec = 1000000 * timeSec
    ctx  <- initConnectionContext
    conn <- timeoutThrow timeUsec $
      connectTo ctx ConnectionParams
        { connectionHostname  = host
        , connectionPort      = port
        , connectionUseSecure = Nothing
        , connectionUseSocks  = Nothing
        }
    when secure $ connectionSetSecure ctx conn def
    pure $ ConnectionWithTimeout conn timeUsec

close :: MonadIO m => HasCallStack => ConnectionWithTimeout -> m ()
close ConnectionWithTimeout{..} = liftIO $ timeoutThrow cwtTimeoutUsec $ connectionClose cwtConnection

recv :: MonadIO m => HasCallStack => ConnectionWithTimeout -> Int -> m (Maybe ByteString)
recv ConnectionWithTimeout{..} = liftIO . (filterMaybe (not . null) <$>) . timeoutThrow cwtTimeoutUsec . connectionGetExact cwtConnection
  where
    filterMaybe :: (a -> Bool) -> a -> Maybe a
    filterMaybe p x | p x       = Just x
                    | otherwise = Nothing

send :: MonadIO m => HasCallStack => ConnectionWithTimeout -> ByteString -> m ()
send ConnectionWithTimeout{..} = liftIO . timeoutThrow cwtTimeoutUsec . connectionPut cwtConnection

sendMany :: MonadIO m => HasCallStack => ConnectionWithTimeout -> [ByteString] -> m ()
sendMany conn@ConnectionWithTimeout{..} chunks = liftIO $ forM_ chunks $ timeoutThrow cwtTimeoutUsec . send conn

timeoutThrow :: HasCallStack => Int -> IO a -> IO a
timeoutThrow timeUsec action = withFrozenCallStack $ do
  res <- timeout timeUsec action
  case res of
    Just a  -> return a
    Nothing -> throwIO TimeOut
