module Database.Bolt.Connection.Pipe where

import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type

import           Control.Monad                      (forM_, unless, void, when)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B (concat, length, null,
                                                          splitAt)
import           Data.Word                          (Word16)
import           Network.Simple.TCP                 (Socket, closeSock,
                                                     connectSock, recv, send,
                                                     sendMany)

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
flush pipe request = do forM_ chunks $ sendMany sock . mkChunk
                        send sock terminal
  where bs        = pack $ toStructure request
        chunkSize = chunkSizeFor (mcs pipe) bs
        chunks    = split chunkSize bs
        terminal  = encodeStrict (0 :: Word16)
        sock      = connectionSocket pipe

        mkChunk :: ByteString -> [ByteString]
        mkChunk chunk = let size = fromIntegral (B.length chunk) :: Word16
                        in  [encodeStrict size, chunk]

fetch :: MonadIO m => Pipe -> m Response
fetch pipe = do bs <- B.concat <$> chunks
                unpack bs >>= fromStructure
  where sock = connectionSocket pipe

        chunks :: MonadIO m => m [ByteString]
        chunks = do size <- decodeStrict <$> recvChunk sock 2
                    chunk <- recvChunk sock size
                    if B.null chunk
                      then return []
                      else do rest <- chunks
                              return (chunk:rest)

-- Helper functions

handshake :: MonadIO m => Pipe -> BoltCfg -> m ()
handshake pipe bcfg = do let sock = connectionSocket pipe
                         send sock (encodeStrict $ magic bcfg)
                         send sock (boltVersionProposal bcfg)
                         serverVersion <- decodeStrict <$> recvChunk sock 4
                         when (serverVersion /= version bcfg) $
                           fail "Unsupported server version"
                         flush pipe (createInit bcfg)
                         response <- fetch pipe
                         unless (isSuccess response) $
                           fail "Authentification failed"
                         return ()

boltVersionProposal :: BoltCfg -> ByteString
boltVersionProposal bcfg = B.concat $ encodeStrict <$> [version bcfg, 0, 0, 0]

recvChunk :: MonadIO m => Socket -> Word16 -> m ByteString
recvChunk sock size = B.concat <$> helper (fromIntegral size)
  where helper :: MonadIO m => Int -> m [ByteString]
        helper 0  = return []
        helper sz = do mbChunk <- recv sock sz
                       case mbChunk of
                         Just chunk -> (chunk:) <$> helper (sz - B.length chunk)
                         Nothing    -> fail "Cannot read chunk from sock"

chunkSizeFor :: Word16 -> ByteString -> Int
chunkSizeFor maxSize bs = 1 + div len noc
  where len = B.length bs
        noc = 1 + div len (fromIntegral maxSize)

split :: Int -> ByteString -> [ByteString]
split size bs | B.null bs = []
              | otherwise = let (chunk, rest) = B.splitAt size bs
                            in chunk : split size rest
