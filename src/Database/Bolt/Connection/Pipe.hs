{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Bolt.Connection.Pipe where

import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type
import qualified Database.Bolt.Connection.Connection as C (close, connect, recv,
                                                           send, sendMany)

import           Control.Exception                   (throwIO)
import           Control.Monad                       (forM_, unless, void, when)
import           Control.Monad.Except                (MonadError (..), ExceptT, runExceptT)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B (concat, length, null,
                                                           splitAt)
import           Data.Word                           (Word16)
import           Network.Connection                  (Connection)

type MonadPipe m = (MonadIO m, MonadError BoltError m)

-- |Creates new 'Pipe' instance to use all requests through
connect :: MonadIO m => BoltCfg -> m Pipe
connect = makeIO connect'
  where
    connect' :: MonadPipe m => BoltCfg -> m Pipe
    connect' bcfg = do conn <- C.connect (secure bcfg) (host bcfg) (fromIntegral $ port bcfg)
                       let pipe = Pipe conn (maxChunkSize bcfg)
                       handshake pipe bcfg
                       pure pipe

-- |Closes 'Pipe'
close :: MonadIO m => Pipe -> m ()
close = C.close . connection

-- |Resets current sessions
reset :: MonadIO m => Pipe -> m ()
reset = makeIO reset'
  where
    reset' :: MonadPipe m => Pipe -> m ()
    reset' pipe = do flush pipe RequestReset
                     response <- fetch pipe
                     when (isFailure response) $
                       throwError ResetFailed

-- Helper to make pipe operations in IO
makeIO :: MonadIO m => (a -> ExceptT BoltError m b) -> a -> m b
makeIO action arg = do actionIO <- runExceptT (action arg)
                       case actionIO of
                         Right x -> pure x
                         Left  e -> liftIO $ throwIO e

-- = Internal interfaces

ackFailure :: MonadPipe m => Pipe -> m ()
ackFailure pipe = flush pipe RequestAckFailure >> void (fetch pipe)

discardAll :: MonadPipe m => Pipe -> m ()
discardAll pipe = flush pipe RequestDiscardAll >> void (fetch pipe)

flush :: MonadPipe m => Pipe -> Request -> m ()
flush pipe request = do forM_ chunks $ C.sendMany conn . mkChunk
                        C.send conn terminal
  where bs        = pack $ toStructure request
        chunkSize = chunkSizeFor (mcs pipe) bs
        chunks    = split chunkSize bs
        terminal  = encodeStrict (0 :: Word16)
        conn      = connection pipe

        mkChunk :: ByteString -> [ByteString]
        mkChunk chunk = let size = fromIntegral (B.length chunk) :: Word16
                        in  [encodeStrict size, chunk]

fetch :: MonadPipe m => Pipe -> m Response
fetch pipe = do bs <- B.concat <$> chunks
                response <- flip unpackAction bs $ unpackT >>= fromStructure
                either (throwError . WrongMessageFormat) pure response
  where conn = connection pipe

        chunks :: MonadPipe m => m [ByteString]
        chunks = do size <- decodeStrict <$> recvChunk conn 2
                    chunk <- recvChunk conn size
                    if B.null chunk
                      then pure []
                      else do rest <- chunks
                              pure (chunk:rest)

-- Helper functions

handshake :: MonadPipe m => Pipe -> BoltCfg -> m ()
handshake pipe bcfg = do let conn = connection pipe
                         C.send conn (encodeStrict $ magic bcfg)
                         C.send conn (boltVersionProposal bcfg)
                         serverVersion <- decodeStrict <$> recvChunk conn 4
                         when (serverVersion /= version bcfg) $
                           throwError UnsupportedServerVersion
                         flush pipe (createInit bcfg)
                         response <- fetch pipe
                         unless (isSuccess response) $
                           throwError AuthentificationFailed

boltVersionProposal :: BoltCfg -> ByteString
boltVersionProposal bcfg = B.concat $ encodeStrict <$> [version bcfg, 0, 0, 0]

recvChunk :: MonadPipe m => Connection -> Word16 -> m ByteString
recvChunk conn size = B.concat <$> helper (fromIntegral size)
  where helper :: MonadPipe m => Int -> m [ByteString]
        helper 0  = pure []
        helper sz = do mbChunk <- C.recv conn sz
                       case mbChunk of
                         Just chunk -> (chunk:) <$> helper (sz - B.length chunk)
                         Nothing    -> throwError CannotReadChunk

chunkSizeFor :: Word16 -> ByteString -> Int
chunkSizeFor maxSize bs = 1 + div len noc
  where len = B.length bs
        noc = 1 + div len (fromIntegral maxSize)

split :: Int -> ByteString -> [ByteString]
split size bs | B.null bs = []
              | otherwise = let (chunk, rest) = B.splitAt size bs
                            in chunk : split size rest
