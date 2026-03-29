{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Connection.RoutingTable
  ( ServerAddress(..), AccessMode(..), RoutingTable(..)
  , parseRoutingTable, parseAddress, isExpired
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import           Database.Bolt.Value.Type (Value(..))

-- | A server address consisting of host and port.
data ServerAddress = ServerAddress
  { serverHost :: String
  , serverPort :: Int
  } deriving (Eq, Ord, Show)

-- | Access mode for routing queries.
data AccessMode = ReadMode | WriteMode
  deriving (Eq, Show)

-- | Routing table obtained from the cluster.
data RoutingTable = RoutingTable
  { rtReaders :: [ServerAddress]
  , rtWriters :: [ServerAddress]
  , rtRouters :: [ServerAddress]
  , rtTTL     :: Int          -- ^ TTL in seconds
  , rtExpiry  :: UTCTime      -- ^ creation time + TTL
  } deriving (Show)

-- | Parse a routing table from a ROUTE response map.
-- Looks for @\"rt\"@ key first (Bolt 4.4+), falls back to top-level @\"ttl\"@/@\"servers\"@ (Bolt 4.3).
parseRoutingTable :: UTCTime -> Map Text Value -> Either Text RoutingTable
parseRoutingTable now m =
  case M.lookup "rt" m of
    Just (M inner) -> parseInner now inner
    _              -> parseInner now m

parseInner :: UTCTime -> Map Text Value -> Either Text RoutingTable
parseInner now m = do
  ttl <- case M.lookup "ttl" m of
           Just (I n) -> Right n
           _          -> Left "Missing or invalid 'ttl' in routing table"
  servers <- case M.lookup "servers" m of
               Just (L xs) -> mapM parseServerGroup xs
               _           -> Left "Missing or invalid 'servers' in routing table"
  let (readers, writers, routers) = partitionServers servers
  if null writers
    then Left "Routing table has no writers"
    else Right RoutingTable
           { rtReaders = readers
           , rtWriters = writers
           , rtRouters = routers
           , rtTTL     = ttl
           , rtExpiry  = addUTCTime (fromIntegral ttl :: NominalDiffTime) now
           }

partitionServers :: [(Text, [ServerAddress])] -> ([ServerAddress], [ServerAddress], [ServerAddress])
partitionServers = foldr go ([], [], [])
  where
    go ("READ",  addrs) (rs, ws, rts) = (addrs ++ rs, ws, rts)
    go ("WRITE", addrs) (rs, ws, rts) = (rs, addrs ++ ws, rts)
    go ("ROUTE", addrs) (rs, ws, rts) = (rs, ws, addrs ++ rts)
    go (_,       _    ) acc           = acc

parseServerGroup :: Value -> Either Text (Text, [ServerAddress])
parseServerGroup (M m) = do
  role <- case M.lookup "role" m of
            Just (T r) -> Right r
            _          -> Left "Missing 'role' in server entry"
  addrs <- case M.lookup "addresses" m of
             Just (L xs) -> mapM parseAddrValue xs
             _           -> Left "Missing 'addresses' in server entry"
  Right (role, addrs)
parseServerGroup _ = Left "Server entry is not a map"

parseAddrValue :: Value -> Either Text ServerAddress
parseAddrValue (T t) = parseAddress t
parseAddrValue _     = Left "Address is not a text value"

-- | Parse an address string like @\"host:port\"@ or @\"[::1]:port\"@ (IPv6).
parseAddress :: Text -> Either Text ServerAddress
parseAddress addr
  | T.null addr = Left "Empty address"
  | T.head addr == '[' =
      -- IPv6: [host]:port
      case T.breakOn "]:" (T.tail addr) of
        (h, rest)
          | T.null rest -> Left ("Invalid IPv6 address: " <> addr)
          | otherwise   ->
              let portStr = T.drop 2 rest  -- drop "]:"
              in case readPort portStr of
                   Just p  -> Right (ServerAddress (T.unpack h) p)
                   Nothing -> Left ("Invalid port in address: " <> addr)
  | otherwise =
      -- Regular: host:port — split on last ':'
      case T.breakOnEnd ":" addr of
        (_, portPart) | T.null portPart -> Left ("No port in address: " <> addr)
        (hostColon, portPart) ->
          case readPort portPart of
            Just p  -> Right (ServerAddress (T.unpack (T.dropEnd 1 hostColon)) p)
            Nothing -> Left ("Invalid port in address: " <> addr)

readPort :: Text -> Maybe Int
readPort t = case reads (T.unpack t) of
               [(n, "")] | n > 0 && n <= 65535 -> Just n
               _ -> Nothing

-- | Check if a routing table has expired.
isExpired :: UTCTime -> RoutingTable -> Bool
isExpired now rt = diffUTCTime now (rtExpiry rt) >= 0
