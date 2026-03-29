{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Bolt.Connection.Type where

import           Database.Bolt.Connection.RoutingTable (AccessMode(..))
import           Database.Bolt.Value.Type              hiding (unpack)

import           Control.DeepSeq                 (NFData(..), rwhnf)
import           Control.Exception               (Exception (..), SomeException, handle)
import           Control.Monad.Catch             (MonadCatch (..), MonadThrow (..))
import           Control.Monad.Trans             (MonadTrans (..), MonadIO (..))
import           Control.Monad.Reader            (MonadReader (..), ReaderT)
import           Control.Monad.Except            (MonadError (..), ExceptT (..))

import           Data.Default                    (Default (..))
import           Data.Map.Strict                 (Map)
import           Data.Monoid                     ()
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
               | RoutingTableUnavailable
               | NoServersAvailable AccessMode
               | RoutingError Text
               | HasCallStack => TimeOut

instance Show BoltError where
  show UnsupportedServerVersion       = "Cannot connect: unsupported server version"
  show AuthentificationFailed         = "Cannot connect: authentification failed"
  show ResetFailed                    = "Cannot reset current pipe: recieved failure from server"
  show CannotReadChunk                = "Cannot fetch: chunk read failed"
  show (WrongMessageFormat msg)       = "Cannot fetch: wrong message format (" <> show msg <> ")"
  show NoStructureInResponse          = "Cannot fetch: no structure in response"
  show (ResponseError re)             = show re
  show (RecordHasNoKey key)           = "Cannot unpack record: key '" <> unpack key <> "' is not presented"
  show (NonHasboltError msg)          = "User error: " <> show msg
  show RoutingTableUnavailable        = "Routing table could not be obtained from server"
  show (NoServersAvailable ReadMode)  = "No servers available for read operations"
  show (NoServersAvailable WriteMode) = "No servers available for write operations"
  show (RoutingError msg)             = "Routing error: " <> unpack msg
  show TimeOut                        = "Operation timeout\n" <> prettyCallStack callStack

instance Exception BoltError

-- |Monad Transformer to do all BOLT actions in
newtype BoltActionT m a = BoltActionT { runBoltActionT :: ReaderT Pipe (ExceptT BoltError m) a }
  deriving (Functor, Applicative, Monad, MonadError BoltError, MonadReader Pipe, MonadThrow, MonadCatch)

instance MonadTrans BoltActionT where
  lift = BoltActionT . lift . lift

instance MonadIO m => MonadIO (BoltActionT m) where
  liftIO = BoltActionT . lift . ExceptT . liftIO . handle (pure . Left . NonHasboltError) . fmap Right

liftE :: Monad m => ExceptT BoltError m a -> BoltActionT m a
liftE = BoltActionT . lift

-- |Configuration of driver connection
data BoltCfg = BoltCfg { magic              :: Word32      -- ^'6060B017' value
                       , version            :: Word32      -- ^Major version number (deafult 0x00070805 for 5.0 through 5.8)
                       , userAgent          :: Text        -- ^Driver user agent
                       , maxChunkSize       :: Word16      -- ^Maximum chunk size of request
                       , socketTimeout      :: Int         -- ^Driver socket timeout in seconds
                       , host               :: String      -- ^Neo4j server hostname
                       , port               :: Int         -- ^Neo4j server port
                       , authType           :: Text        -- ^Neo4j auth schema
                       , user               :: Text        -- ^Neo4j user
                       , password           :: Text        -- ^Neo4j password
                       , secure             :: Bool        -- ^Use TLS or not
                       , notifMinSeverity   :: Maybe Text  -- ^Min notification severity: @"OFF"@, @"WARNING"@, @"INFORMATION"@
                       , notifDisabledClass :: [Text]      -- ^Disabled notification categories\/classifications
                       , database           :: Maybe Text  -- ^Target database name (Nothing = default)
                       }
  deriving (Eq, Show, Read)

instance Default BoltCfg where
  def = BoltCfg { magic              = 1616949271
                , version            = 0x00070805
                , userAgent          = "hasbolt/1.8"
                , maxChunkSize       = 65535
                , socketTimeout      = 5
                , host               = "127.0.0.1"
                , port               = 7687
                , authType           = "basic"
                , user               = ""
                , password           = ""
                , secure             = False
                , notifMinSeverity   = Nothing
                , notifDisabledClass = []
                , database           = Nothing
                }

data ConnectionWithTimeout
  = ConnectionWithTimeout
      { cwtConnection  :: !Connection
      , cwtTimeoutUsec :: !Int
        -- ^ Timeout in microseconds
      }

data Pipe = Pipe { connection                          :: ConnectionWithTimeout
                 -- ^ Driver connection socket
                 , mcs                                 :: Word16
                 -- ^ Driver maximum chunk size of request
                 , pipe_version                        :: Word32
                 -- ^ Connection version 0000mnMJ
                 , pipeNotificationsMinimumSeverity    :: Maybe Text
                 -- ^ Notification minimum severity
                 , pipeNotificationsDisabledCategories :: [Text]
                 -- ^ Disabled notification categories\/classifications
                 , pipeDatabase                        :: Maybe Text
                 -- ^ Target database name
                 }

instance NFData Pipe where
  rnf = rwhnf

data AuthToken = AuthToken { scheme      :: Text
                           , principal   :: Text
                           , credentials :: Text
                           }
  deriving (Eq)

instance Show AuthToken where
  show at = "AuthToken {scheme = " <> show (scheme at)
         <> ", principal = " <> show (principal at)
         <> ", credentials = \"<redacted>\"}"

data Response = ResponseSuccess { succMap   :: Map Text Value }
              | ResponseRecord  { recsList  :: [Value] }
              | ResponseIgnored
              | ResponseFailure { failMap   :: Map Text Value }
  deriving (Eq, Show)

data Request = RequestInit
                 { agent       :: Text
                 , token       :: AuthToken
                 , initVersion :: Word32
                 , initRouting :: Maybe (Map Text Value)  -- ^Optional routing context for HELLO
                 }
             | RequestRun
                 { statement  :: Text
                 , parameters :: Map Text Value
                 }
             | RequestRunV3
                 { statement  :: Text
                 , parameters :: Map Text Value
                 , extra      :: Map Text Value
                 }
             | RequestAckFailure
             | RequestReset
             | RequestDiscardAll
             | RequestPullAll
             | RequestGoodbye
               -- | Introduced in v3.
             | RequestBegin
                 { extra       :: Map Text Value
                 }
               -- | Introduced in v3.
             | RequestCommit
               -- | Introduced in v3.
             | RequestRollback
               -- | Introduced in v5.1. Sends auth credentials separately from HELLO.
             | RequestLogon
                 { logonToken  :: AuthToken
                 }
               -- | Introduced in v5.1. Sent before GOODBYE on close.
             | RequestLogoff
               -- | Introduced in v5.4. Reports driver API usage.
             | RequestTelemetry
                 { telemetryApi :: Int
                 }
               -- | Introduced in v4/v5. PULL with extra dict (e.g. @{n: -1}@).
             | RequestPull
                 { pullExtra   :: Map Text Value
                 }
               -- | Introduced in v4/v5. DISCARD with extra dict (e.g. @{n: -1}@).
             | RequestDiscard
                 { discardExtra :: Map Text Value
                 }
              -- | Introduced in v4.3. Requests routing table from server.
             | RequestRoute
                 { routeContext   :: Map Text Value  -- routing context dict
                 , routeBookmarks :: [Text]          -- transaction bookmarks
                 , routeExtra     :: Map Text Value  -- e.g. @{\"db\": \"neo4j\"}@
                 }
  deriving (Eq, Show)
