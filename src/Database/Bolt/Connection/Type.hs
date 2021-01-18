{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Bolt.Connection.Type where

import           Database.Bolt.Value.Type hiding (unpack)

import           Control.Exception               (Exception (..), SomeException, handle)
import           Control.Monad.Trans             (MonadTrans (..), MonadIO (..))
import           Control.Monad.Reader            (MonadReader (..), ReaderT)
import           Control.Monad.Except            (MonadError (..), ExceptT (..))

import           Data.Default                    (Default (..))
import           Data.Map.Strict                 (Map)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text, unpack)
import           Data.Word                       (Word16, Word32)
import           GHC.Stack                       (HasCallStack, callStack, prettyCallStack)
import           Network.Connection              (Connection)


-- |Error obtained from BOLT server
data ResponseError = KnownResponseFailure Text Text
                   | UnknownResponseFailure
  deriving (Eq, Ord)

instance Show ResponseError where
  show (KnownResponseFailure tpe msg) = unpack tpe <> ": " <> unpack msg
  show UnknownResponseFailure         = "Unknown response error"

-- |Error that can appear during 'BoltActionT' manipulations
data BoltError = UnsupportedServerVersion
               | AuthentificationFailed
               | ResetFailed
               | CannotReadChunk
               | WrongMessageFormat UnpackError
               | NoStructureInResponse
               | ResponseError ResponseError
               | RecordHasNoKey Text
               | NonHasboltError SomeException
               | HasCallStack => TimeOut

instance Show BoltError where
  show UnsupportedServerVersion = "Cannot connect: unsupported server version"
  show AuthentificationFailed   = "Cannot connect: authentification failed"
  show ResetFailed              = "Cannot reset current pipe: recieved failure from server"
  show CannotReadChunk          = "Cannot fetch: chunk read failed"
  show (WrongMessageFormat msg) = "Cannot fetch: wrong message format (" <> show msg <> ")"
  show NoStructureInResponse    = "Cannot fetch: no structure in response"
  show (ResponseError re)       = show re
  show (RecordHasNoKey key)     = "Cannot unpack record: key '" <> unpack key <> "' is not presented"
  show (NonHasboltError msg)    = "User error: " <> show msg
  show TimeOut                  = "Operation timeout\n" <> prettyCallStack callStack

instance Exception BoltError

-- |Monad Transformer to do all BOLT actions in
newtype BoltActionT m a = BoltActionT { runBoltActionT :: ReaderT Pipe (ExceptT BoltError m) a }
  deriving (Functor, Applicative, Monad, MonadError BoltError, MonadReader Pipe)

instance MonadTrans BoltActionT where
  lift = BoltActionT . lift . lift

instance MonadIO m => MonadIO (BoltActionT m) where
  liftIO = BoltActionT . lift . ExceptT . liftIO . handle (pure . Left . NonHasboltError) . fmap Right

liftE :: Monad m => ExceptT BoltError m a -> BoltActionT m a
liftE = BoltActionT . lift

-- |Configuration of driver connection
data BoltCfg = BoltCfg { magic         :: Word32  -- ^'6060B017' value
                       , version       :: Word32  -- ^'00000001' value
                       , userAgent     :: Text    -- ^Driver user agent
                       , maxChunkSize  :: Word16  -- ^Maximum chunk size of request
                       , socketTimeout :: Int     -- ^Driver socket timeout in seconds
                       , host          :: String  -- ^Neo4j server hostname
                       , port          :: Int     -- ^Neo4j server port
                       , user          :: Text    -- ^Neo4j user
                       , password      :: Text    -- ^Neo4j password
                       , secure        :: Bool    -- ^Use TLS or not
                       }
  deriving (Eq, Show, Read)

instance Default BoltCfg where
  def = BoltCfg { magic         = 1616949271
                , version       = 1
                , userAgent     = "hasbolt/1.4"
                , maxChunkSize  = 65535
                , socketTimeout = 5
                , host          = "127.0.0.1"
                , port          = 7687
                , user          = ""
                , password      = ""
                , secure        = False
                }

data ConnectionWithTimeout
  = ConnectionWithTimeout
      { cwtConnection  :: !Connection
      , cwtTimeoutUsec :: !Int
        -- ^ Timeout in microseconds
      }

data Pipe = Pipe { connection :: ConnectionWithTimeout -- ^Driver connection socket
                 , mcs        :: Word16                -- ^Driver maximum chunk size of request
                 }

data AuthToken = AuthToken { scheme      :: Text
                           , principal   :: Text
                           , credentials :: Text
                           }
  deriving (Eq, Show)

data Response = ResponseSuccess { succMap   :: Map Text Value }
              | ResponseRecord  { recsList  :: [Value] }
              | ResponseIgnored { ignoreMap :: Map Text Value }
              | ResponseFailure { failMap   :: Map Text Value }
  deriving (Eq, Show)

data Request = RequestInit { agent :: Text
                           , token :: AuthToken
                           }
             | RequestRun  { statement  :: Text
                           , parameters :: Map Text Value
                           }
             | RequestAckFailure
             | RequestReset
             | RequestDiscardAll
             | RequestPullAll
  deriving (Eq, Show)
