module Database.Bolt.Connection.Connection where

import           Control.Applicative    (pure, (<$>))
import           Control.Monad          (when, forM_)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString, null)
import           Data.Default           (Default (..))
import           Network.Socket         (PortNumber)
import           Network.Connection     (Connection, ConnectionParams (..), connectTo, connectionSetSecure,
                                         initConnectionContext, connectionClose, connectionGetExact, connectionPut)
import           Prelude                hiding (null)

connect :: MonadIO m => Bool -> String -> PortNumber -> m Connection
connect secure host port = liftIO $ do
                              ctx  <- initConnectionContext
                              conn <- connectTo ctx ConnectionParams { connectionHostname  = host
                                                                     , connectionPort      = port
                                                                     , connectionUseSecure = Nothing
                                                                     , connectionUseSocks  = Nothing
                                                                     }
                              when secure $ connectionSetSecure ctx conn def
                              pure conn

close :: MonadIO m => Connection -> m ()
close = liftIO . connectionClose

recv :: MonadIO m => Connection -> Int -> m (Maybe ByteString)
recv conn = liftIO . (filterMaybe (not . null) <$>) . connectionGetExact conn
  where
    filterMaybe :: (a -> Bool) -> a -> Maybe a
    filterMaybe p x | p x       = Just x
                    | otherwise = Nothing

send :: MonadIO m => Connection -> ByteString -> m ()
send conn = liftIO . connectionPut conn

sendMany :: MonadIO m => Connection -> [ByteString] -> m ()
sendMany conn chunks = forM_ chunks $ send conn
