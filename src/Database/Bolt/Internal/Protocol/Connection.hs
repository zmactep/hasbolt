module Database.Bolt.Internal.Protocol.Connection
    ( connect, close, reset
    , flush, fetch, processFailure
    ) where

import           Control.Monad
import           Control.Monad.IO.Class                     (MonadIO (..),
                                                             liftIO)
import           Control.Monad.Trans.State
import           Data.Binary                                (Binary (..),
                                                             decode, encode)
import           Data.ByteString                            (ByteString, append,
                                                             cons, snoc)
import qualified Data.ByteString                            as B (concat, empty,
                                                                  length, null,
                                                                  splitAt)
import           Data.ByteString.Lazy                       (fromStrict,
                                                             toStrict)
import           Data.Hex
import           Data.Maybe                                 (fromMaybe)
import           Data.Text                                  (Text)
import           Data.Word                                  (Word16, Word32)
import           Database.Bolt.Internal.Protocol.Request
import           Database.Bolt.Internal.Protocol.Response
import           Database.Bolt.Internal.Protocol.Types
import           Database.Bolt.Internal.Unpack.UnpackStream (runUnpackT)
import           Network.Simple.TCP                         (Socket, closeSock,
                                                             connectSock, recv,
                                                             send)
import           Network.Socket                             (isConnected)

connect :: MonadIO m => BoltCfg -> m Pipe
connect bcfg = do (sock, addr) <- connectSock (host bcfg) (show $ port bcfg)
                  let pipe = Pipe sock (maxChunkSize bcfg)
                  send sock (toStrict . encode . magic $ bcfg)
                  send sock (boltClientVersionProposal bcfg)
                  serverVersion <- liftIO (recvDecoded sock 4 0 :: IO Word32)
                  when (serverVersion /= version bcfg) $
                    fail "Server version is incorrect"
                  flush pipe $ initByConfig bcfg
                  resp <- fetch pipe
                  unless (isSuccess resp) $
                    fail "Authentification error"
                  return pipe

close :: MonadIO m => Pipe -> m ()
close = closeSock . connectionSocket

flush :: MonadIO m => Pipe -> Request -> m ()
flush pipe req = do let chunkSize = fromIntegral $ mcs pipe :: Int
                    let chunks = splitInChunks chunkSize (pack req)
                    let sizedChunkSet = B.concat $ addSizeToChunk <$> chunks
                    send (connectionSocket pipe) (sizedChunkSet `snoc` 0 `snoc` 0)

fetch :: MonadIO m => Pipe -> m Response
fetch pipe = liftIO (execStateT fetchST B.empty) >>= runUnpackT
  where fetchST :: StateT ByteString IO ()
        fetchST = do let sock = connectionSocket pipe
                     chunkSize <- recvDecoded sock 2 0 :: StateT ByteString IO Word16
                     when (chunkSize /= 0) $ do
                       chunk <- liftIO $ fromMaybe B.empty <$> recv sock (fromIntegral chunkSize)
                       modify (`append` chunk)
                       fetchST
                     return ()

processFailure :: MonadIO m => Pipe -> m ()
processFailure pipe = do flush pipe RequestAckFailure
                         _ <- fetch pipe
                         return ()

reset :: MonadIO m => Pipe -> m Bool
reset pipe = do flush pipe RequestReset
                resp <- fetch pipe
                case resp of
                  (ResponseSuccess _) -> return True
                  (ResponseFailure _) -> return False
                  _                   -> fail "Server sent something strange"

checkConnection :: MonadIO m => Socket -> m ()
checkConnection sock = do status <- liftIO $ isConnected sock
                          unless status $
                            fail "Connection failed"

recvDecoded :: (MonadIO m, Binary a) => Socket -> Int -> a -> m a
recvDecoded sock size def = do raw <- liftIO $ recv sock size
                               return $ fromMaybe def $ decode . fromStrict <$> raw

boltClientVersionProposal :: BoltCfg -> ByteString
boltClientVersionProposal bcfg = B.concat $ toStrict . encode <$> [version bcfg, 0, 0, 0]

addSizeToChunk :: ByteString -> ByteString
addSizeToChunk bs = let sz = fromIntegral (B.length bs) :: Word16
                    in toStrict (encode sz) `append` bs

splitInChunks :: Int -> ByteString -> [ByteString]
splitInChunks mx bs | B.null bs = []
                    | otherwise = let (hd, rest) = B.splitAt mx bs
                                  in hd : splitInChunks mx rest
