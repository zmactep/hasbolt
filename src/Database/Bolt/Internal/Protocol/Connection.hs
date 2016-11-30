module Database.Bolt.Internal.Protocol.Connection
    ( connect, close
    ) where

import           Control.Monad
import           Control.Monad.IO.Class                  (MonadIO (..), liftIO)
import           Control.Monad.Trans.State
import           Data.Binary                             (Binary (..), decode,
                                                          encode)
import           Data.ByteString                         (ByteString, append)
import qualified Data.ByteString                         as B (concat, empty)
import           Data.ByteString.Lazy                    (fromStrict, toStrict)
import           Data.Maybe                              (fromMaybe)
import           Data.Word                               (Word16, Word32)
import           Database.Bolt.Internal.Protocol.Request
import           Database.Bolt.Internal.Protocol.Types
import           Network.Simple.TCP                      (Socket, closeSock,
                                                          connectSock, recv,
                                                          send)

connect :: BoltCfg -> IO Pipe
connect bcfg = do (sock, addr) <- connectSock (host bcfg) (show $ port bcfg)
                  send sock (toStrict . encode . magic $ bcfg)
                  send sock (boltClientVersionProposal bcfg)
                  serverVersion <- recvDecoded sock 4 0 :: IO Word32
                  when (serverVersion /= version bcfg) $
                    fail "Server version is incorrect"
                  send sock $ pack (initByConfig bcfg)
                  bs <- fetch sock
                  return $ Pipe sock

close :: Pipe -> IO ()
close = closeSock . connectionSocket

fetch :: Socket -> IO ByteString
fetch sock = execStateT fetchST B.empty
  where fetchST :: StateT ByteString IO ()
        fetchST = do chunkSize <- recvDecoded sock 2 0 :: StateT ByteString IO Word16
                     when (chunkSize /= 0) $ do
                       bs <- recvDecoded sock (fromIntegral chunkSize) B.empty :: StateT ByteString IO ByteString
                       modify (`append` bs)
                       fetchST

recvDecoded :: (MonadIO m, Binary a) => Socket -> Int -> a -> m a
recvDecoded sock size def = do raw <- liftIO $ recv sock size
                               return $ fromMaybe def $ decode . fromStrict <$> raw

boltClientVersionProposal :: BoltCfg -> ByteString
boltClientVersionProposal bcfg = B.concat $ toStrict . encode <$> [version bcfg, 0, 0, 0]
