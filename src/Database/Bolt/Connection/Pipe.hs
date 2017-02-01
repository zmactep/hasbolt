{-# LANGUAGE ScopedTypeVariables #-}

module Database.Bolt.Connection.Pipe where

import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type

import           Control.Monad                      (forM_, unless, void, when)
import           Control.Monad.IO.Class             (MonadIO (..), liftIO)
import           Data.Binary                        (Binary (..))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B (concat, empty, length,
                                                          null, splitAt)
import           Data.Maybe                         (fromMaybe)
import           Data.Word                          (Word16, Word32)
import           Network.Simple.TCP                 (Socket, closeSock,
                                                     connectSock, recv, send)

-- |Creates new 'Pipe' instance to use all requests through
connect :: MonadIO m => BoltCfg -> m Pipe
connect bcfg = do (sock, _) <- connectSock (host bcfg) (show $ port bcfg)
                  let pipe = Pipe sock (maxChunkSize bcfg)
                  handshake pipe bcfg
                  return pipe

-- |Closes 'Pipe'
close :: MonadIO m => Pipe -> m ()
close = closeSock . connectionSocket

-- |Resets current sessions
reset :: MonadIO m => Pipe -> m ()
reset pipe = do flush pipe RequestReset
                response <- fetch pipe
                when (isFailure response) $
                  fail "Reset failed"
                return ()

ackFailure :: MonadIO m => Pipe -> m ()
ackFailure pipe = flush pipe RequestAckFailure >> void (fetch pipe)

discardAll :: MonadIO m => Pipe -> m ()
discardAll pipe = flush pipe RequestDiscardAll >> void (fetch pipe)

flush :: MonadIO m => Pipe -> Request -> m ()
flush pipe request = do let chunkSize = fromIntegral (mcs pipe)
                        let chunks = split chunkSize (pack $ toStructure request)
                        let terminal = encodeStrict (0 :: Word16)
                        let sock = connectionSocket pipe
                        forM_ chunks $ \chunk -> do
                          let size = fromIntegral (B.length chunk) :: Word16
                          send sock $ encodeStrict size
                          send sock chunk
                        send sock terminal
  where split :: Int -> ByteString -> [ByteString]
        split size bs | B.null bs = []
                      | otherwise = let (chunk, rest) = B.splitAt size bs
                                    in chunk : split size rest

fetch :: MonadIO m => Pipe -> m Response
fetch pipe = do bs <- B.concat <$> chunks
                unpack bs >>= fromStructure
  where sock = connectionSocket pipe

        chunks :: MonadIO m => m [ByteString]
        chunks = do chunk <- recvWord sock 2 >>= recvChunk sock
                    if B.null chunk then return [] else (chunk:) <$> chunks

-- Helper functions

handshake :: MonadIO m => Pipe -> BoltCfg -> m ()
handshake pipe bcfg = do let sock = connectionSocket pipe
                         send sock (encodeStrict $ magic bcfg)
                         send sock (boltVersionProposal bcfg)
                         serverVersion :: Word32 <- recvWord sock 4
                         when (serverVersion /= version bcfg) $
                           fail "Unsupported server version"
                         flush pipe (createInit bcfg)
                         response <- fetch pipe
                         unless (isSuccess response) $
                           fail "Authentification failed"
                         return ()

boltVersionProposal :: BoltCfg -> ByteString
boltVersionProposal bcfg = B.concat $ encodeStrict <$> [version bcfg, 0, 0, 0]

recvWord :: (MonadIO m, Binary a, Integral a) => Socket -> Int -> m a
recvWord sock size = do bs <- liftIO $ recv sock size
                        return (fromMaybe 0 $ decodeStrict <$> bs)

recvChunk :: MonadIO m => Socket -> Word16 -> m ByteString
recvChunk _    0    = return B.empty
recvChunk sock size = do let isize = fromIntegral size
                         fromMaybe B.empty <$> liftIO (recv sock isize)
