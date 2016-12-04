{-# LANGUAGE ScopedTypeVariables #-}

module Database.Bolt.Connection.Pipe where

import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type

import           Control.Monad                       (unless, void, when)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Binary                         (Binary (..))
import           Data.ByteString                     (ByteString, append)
import qualified Data.ByteString                     as B (concat, length, null,
                                                           splitAt, empty)
import           Data.Maybe                          (fromMaybe)
import           Data.Word                           (Word16, Word32)
import           Network.Simple.TCP                  (closeSock, connectSock,
                                                      recv, send)
import           Network.Socket                      (isConnected)

connect :: MonadIO m => BoltCfg -> m Pipe
connect bcfg = do (sock, _) <- connectSock (host bcfg) (show $ port bcfg)
                  let pipe = Pipe sock (maxChunkSize bcfg)
                  return pipe

close :: MonadIO m => Pipe -> m ()
close = closeSock . connectionSocket

reset :: MonadIO m => Pipe -> m ()
reset pipe = do flush pipe RequestReset
                response <- fetch pipe
                when (isFailure response) $
                  fail "Reset failed"
                return ()

ackFailure :: MonadIO m => Pipe -> m ()
ackFailure pipe = flush pipe RequestAckFailure >> void (fetch pipe)

flush :: MonadIO m => Pipe -> Request -> m ()
flush pipe request = do let chunkSize = fromIntegral (mcs pipe)
                        let chunks = split chunkSize (pack $ toStructure request)
                        let packet = B.concat $ consSize <$> chunks
                        let terminal = encodeStrict (0 :: Word16)
                        send (connectionSocket pipe) (packet `append` terminal)
  where split :: Int -> ByteString -> [ByteString]
        split size bs | B.null bs = []
                      | otherwise = let (chunk, rest) = B.splitAt size bs
                                    in chunk : split size rest

        consSize :: ByteString -> ByteString
        consSize bs = let size = fromIntegral (B.length bs) :: Word16
                      in encodeStrict size `append` bs

fetch :: MonadIO m => Pipe -> m Response
fetch pipe = do bs <- fetchHelper
                unpack bs >>= fromStructure
  where fetchHelper :: MonadIO m => m ByteString
        fetchHelper = do chunkSize :: Word16 <- recvWord pipe 2
                         case chunkSize of
                           0 -> return B.empty
                           _ -> do chunk <- recvChunk pipe chunkSize
                                   rest  <- fetchHelper
                                   return $ chunk `append` rest

-- Helper functions

handshake :: MonadIO m => Pipe -> BoltCfg -> m ()
handshake pipe bcfg = do let sock = connectionSocket pipe
                         send sock (encodeStrict $ magic bcfg)
                         send sock (boltVersionProposal bcfg)
                         serverVersion :: Word32 <- recvWord pipe 4
                         when (serverVersion /= version bcfg) $
                           fail "Unsupported server version"
                         flush pipe (createInit bcfg)
                         response <- fetch pipe
                         unless (isSuccess response) $
                           fail "Authentification failed"
                         return ()

boltVersionProposal :: BoltCfg -> ByteString
boltVersionProposal bcfg = B.concat $ encodeStrict <$> [version bcfg, 0, 0, 0]

recvWord :: (MonadIO m, Binary a, Integral a) => Pipe -> Int -> m a
recvWord pipe size = do let sock = connectionSocket pipe
                        bs <- liftIO $ recv sock size
                        return (fromMaybe 0 $ decodeStrict <$> bs)

recvChunk :: MonadIO m => Pipe -> Word16 -> m ByteString
recvChunk pipe size = do let sock = connectionSocket pipe
                         fromMaybe B.empty <$> liftIO (recv sock (fromIntegral size))
