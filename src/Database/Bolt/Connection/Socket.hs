module Database.Bolt.Connection.Socket
  ( Socket
  , connectSock, closeSock
  , recv, send, sendMany
  ) where

import           Control.Exception         (bracketOnError)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.ByteString           (ByteString, null)
import           Network.Socket            (AddrInfoFlag (..), HostName,
                                            ServiceName, SockAddr, Socket,
                                            SocketType (..), addrAddress,
                                            addrFamily, addrFlags, addrProtocol,
                                            addrSocketType, defaultHints,
                                            getAddrInfo, socket, close, connect)
import qualified Network.Socket.ByteString as NSB (recv, sendAll, sendMany)
import           Prelude                   hiding (null)

connectSock :: MonadIO m => HostName -> ServiceName -> m (Socket, SockAddr)
connectSock host port = liftIO $ bracketOnError (createSock host port) (closeSock . fst) $
                          \(sock, addr) -> liftIO (connect sock addr) >> pure (sock, addr)

createSock :: MonadIO m => HostName -> ServiceName -> m (Socket, SockAddr)
createSock host port = liftIO $ do (addr:_) <- getAddrInfo (Just hints) (Just host) (Just port)
                                   let family'   = addrFamily addr
                                   let type'     = addrSocketType addr
                                   let protocol' = addrProtocol addr
                                   sock <- socket family' type' protocol'
                                   pure (sock, addrAddress addr)
  where hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrSocketType = Stream
                             }

closeSock :: MonadIO m => Socket -> m ()
closeSock = liftIO . close

recv :: MonadIO m => Socket -> Int -> m (Maybe ByteString)
recv sock nbytes = liftIO $ do bs <- NSB.recv sock nbytes
                               if null bs then pure Nothing
                                            else pure (Just bs)

send :: MonadIO m => Socket -> ByteString -> m ()
send sock = liftIO . NSB.sendAll sock

sendMany :: MonadIO m => Socket -> [ByteString] -> m ()
sendMany sock = liftIO . NSB.sendMany sock
