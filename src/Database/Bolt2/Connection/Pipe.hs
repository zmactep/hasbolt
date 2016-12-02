module Database.Bolt2.Connection.Pipe where

import           Database.Bolt2.Connection.Type
import           Database.Bolt2.Value.Type

import           Control.Monad.IO.Class         (MonadIO (..))
import           Network.Simple.TCP             (closeSock, connectSock, recv,
                                                 send)
import           Network.Socket                 (isConnected)

connect :: MonadIO m => BoltCfg -> m Pipe
connect = undefined

close :: MonadIO m => Pipe -> m ()
close = closeSock . connectionSocket

reset :: MonadIO m => Pipe -> m ()
reset = undefined

ackFailure :: MonadIO m => Pipe -> m ()
ackFailure = undefined

flush :: MonadIO m => Pipe -> Request -> m ()
flush = undefined

fetch :: MonadIO m => Pipe -> m Response
fetch = undefined
