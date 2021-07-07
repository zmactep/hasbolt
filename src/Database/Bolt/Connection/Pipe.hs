{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Bolt.Connection.Pipe where

import qualified Database.Bolt.Connection.Connection as C (close, connect, recv, send, sendMany)
import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type            (BoltValue (pack, unpackT),
                                                      FromStructure (fromStructure),
                                                      ToStructure (toStructure), unpackAction)

import           Control.Exception                   (throwIO)
import           Control.Monad                       (forM_, unless, void, when)
import           Control.Monad.Except                (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.Trans                 (MonadIO (..))
import           Data.Binary                         (decode)
import           Data.Binary.Put                     (runPut)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B (concat, length)
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Internal       as BSL
import           Data.Int                            (Int64)
import           Data.Word                           (Word16)
import           GHC.Stack                           (HasCallStack)

type MonadPipe m = (MonadIO m, MonadError BoltError m)

-- |Creates new 'Pipe' instance to use all requests through
connect :: MonadIO m => HasCallStack => BoltCfg -> m Pipe
connect = makeIO connect'
  where
    connect' :: MonadPipe m => BoltCfg -> m Pipe
    connect' bcfg = do conn <- C.connect (secure bcfg) (host bcfg) (fromIntegral $ port bcfg) (socketTimeout bcfg)
                       let pipe = Pipe conn (maxChunkSize bcfg) (version bcfg)
                       handshake pipe bcfg
                       pure pipe

-- |Closes 'Pipe'
close :: MonadIO m => HasCallStack => Pipe -> m ()
close pipe = do when (isNewVersion $ pipe_version pipe) $ makeIO (`flush` RequestGoodbye) pipe
                C.close $ connection pipe

-- |Resets current sessions
reset :: MonadIO m => HasCallStack => Pipe -> m ()
reset = makeIO reset'
  where
    reset' :: MonadPipe m => Pipe -> m ()
    reset' pipe = do flush pipe RequestReset
                     response <- fetch pipe
                     when (isFailure response) $
                       throwError ResetFailed

-- Helper to make pipe operations in IO
makeIO :: MonadIO m => HasCallStack => (a -> ExceptT BoltError m b) -> a -> m b
makeIO action arg = do actionIO <- runExceptT (action arg)
                       case actionIO of
                         Right x -> pure x
                         Left  e -> liftIO $ throwIO e

-- = Internal interfaces

-- |Processes error via ackFailure or reset
processError :: MonadIO m => HasCallStack => Pipe -> m ()
processError pipe@Pipe{..} = if isNewVersion pipe_version
                               then reset pipe
                               else makeIO ackFailure pipe

ackFailure :: MonadPipe m => HasCallStack => Pipe -> m ()
ackFailure pipe = flush pipe RequestAckFailure >> void (fetch pipe)

discardAll :: MonadPipe m => HasCallStack => Pipe -> m ()
discardAll pipe = flush pipe RequestDiscardAll >> void (fetch pipe)

flush :: MonadPipe m => HasCallStack => Pipe -> Request -> m ()
flush pipe request = do forM_ chunks $ C.sendMany conn . mkChunk
                        C.send conn terminal
  where bs        = runPut $ pack $ toStructure request
        chunkSize = chunkSizeFor (mcs pipe) bs
        chunks    = split chunkSize bs
        terminal  = encodeStrict (0 :: Word16)
        conn      = connection pipe

        mkChunk :: ByteString -> [ByteString]
        mkChunk chunk = let size = fromIntegral (B.length chunk) :: Word16
                        in  [encodeStrict size, chunk]

fetch :: MonadPipe m => HasCallStack => Pipe -> m Response
fetch pipe = do bs <- chunks
                let response = unpackAction unpackT bs >>= fromStructure
                case response of
                  Left ue -> throwError $ WrongMessageFormat ue
                  Right a -> pure a

  where conn = connection pipe

        chunks :: MonadPipe m => m BSL.ByteString
        chunks = do size <- decode <$> recvChunk conn 2
                    chunk <- recvChunk conn size
                    if BSL.null chunk
                      then pure BSL.empty
                      else do rest <- chunks
                              pure $! chunk <> rest

-- Helper functions

handshake :: MonadPipe m => HasCallStack => Pipe -> BoltCfg -> m ()
handshake pipe bcfg = do let conn = connection pipe
                         C.send conn (encodeStrict $ magic bcfg)
                         C.send conn (boltVersionProposal bcfg)
                         serverVersion <- decode <$> recvChunk conn 4
                         when (serverVersion /= version bcfg) $
                           throwError UnsupportedServerVersion
                         flush pipe (createInit bcfg)
                         response <- fetch pipe
                         unless (isSuccess response) $
                           throwError AuthentificationFailed

boltVersionProposal :: BoltCfg -> ByteString
boltVersionProposal bcfg = B.concat $ encodeStrict <$> [version bcfg, 0, 0, 0]

recvChunk :: MonadPipe m => HasCallStack => ConnectionWithTimeout -> Word16 -> m BSL.ByteString
recvChunk conn size = helper (fromIntegral size)
  where helper :: MonadPipe m => Int -> m BSL.ByteString
        helper 0  = pure BSL.empty
        helper sz = do mbChunk <- C.recv conn sz
                       case mbChunk of
                         Just chunk -> BSL.chunk chunk <$> helper (sz - B.length chunk)
                         Nothing    -> throwError CannotReadChunk

chunkSizeFor :: Word16 -> BSL.ByteString -> Int64
chunkSizeFor maxSize bs = 1 + div len noc
  where len = BSL.length bs
        noc = 1 + div len (fromIntegral maxSize)

split :: Int64 -> BSL.ByteString -> [ByteString]
split size bs | BSL.null bs = []
              | otherwise = let (chunk, rest) = BSL.splitAt size bs
                            in BSL.toStrict chunk : split size rest
