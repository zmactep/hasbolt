{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- A connection pool for Neo4j clusters that automatically discovers cluster
-- topology via the BOLT routing protocol and manages pooled connections to
-- individual servers.
--
-- == Usage
--
-- @
-- import Database.Bolt (BoltCfg(..), BoltActionT)
-- import Database.Bolt.Connection.RouterPool
-- import Data.Default (def)
--
-- main :: IO ()
-- main = do
--     let cfg = def { rpcBoltCfg = def { host = \"neo4j-core-1\", port = 7687
--                                      , user = \"neo4j\", password = \"secret\" } }
--     pool <- connectRouterPool cfg
--     -- Write query (routed to a writer)
--     runRouterPool pool $ query \"CREATE (n:Person {name: \$name})\" (props [\"name\" =: \"Alice\"])
--     -- Read query (routed to a reader replica)
--     records <- runRouterPoolRead pool $ query \"MATCH (n:Person) RETURN n\" mempty
--     closeRouterPool pool
-- @
--
-- == Configuration
--
-- Use 'RouterPoolCfg' (has a 'Default' instance) to control pool behaviour:
--
--   * 'rpcBoltCfg' — the 'BoltCfg' used for all connections. The @host@ and @port@
--     serve as the bootstrap address for initial topology discovery.
--   * 'rpcMaxPerServer' — soft cap on connections per server (default 10). Excess
--     pipes are closed on release rather than returned to the idle list.
--   * 'rpcIdleTimeout' — seconds before an idle pipe is reaped (default 30).
--   * 'rpcReapInterval' — seconds between background reaper sweeps (default 10).
--
-- == Caveats
--
--   * Requires BOLT v5+ (the pool sends @ROUTE@ requests for topology discovery).
--   * The bootstrap address must be reachable at pool creation time; if the initial
--     @ROUTE@ request fails, 'connectRouterPool' throws immediately.
--   * In-use pipes are not closed by 'closeRouterPool' — they are closed when the
--     'BoltActionT' that holds them completes (successfully or not).
--   * Routing table refresh failures are silently ignored; the pool continues with
--     the stale table until the next refresh attempt succeeds.
--
-- == Internals
--
-- The pool maintains a 'Map' from 'ServerAddress' to per-server state (idle pipe
-- list and in-use count). Pipe acquisition uses __least-connections__ selection
-- with __round-robin tie-breaking__: the eligible server list is rotated by a
-- monotonically increasing index before sorting by in-use count, so servers with
-- equal load are picked in round-robin order.
--
-- On connect failure during acquisition, the pool falls back through the ranked
-- server list, shifting the in-use reservation from the failed server to the next
-- candidate. Async exceptions are always re-thrown immediately.
--
-- A background __reaper__ thread periodically closes idle pipes that have exceeded
-- 'rpcIdleTimeout'. Routing table __refresh__ is triggered on each 'acquirePipe'
-- when the TTL has expired, with a non-blocking lock so that at most one thread
-- fetches a new table while others proceed with the current (stale) table.
-- When a new routing table arrives, the pool reconciles its server map: pipes to
-- removed servers are closed, new servers get empty pools, and existing servers
-- retain their pipes.
module Database.Bolt.Connection.RouterPool
  ( RouterPool
  , RouterPoolCfg(..)
  , connectRouterPool, closeRouterPool
  , runRouterPool, runRouterPoolE
  , runRouterPoolRead, runRouterPoolReadE
  , getRoutingTable
    -- * Internals exported for testing
  , isConnectionError
  , reconcileState
  , PoolState(..), ServerPool(..), IdlePipe(..)
  ) where

import           Database.Bolt.Connection              (runE, sendRawRequest)
import           Database.Bolt.Connection.Instances    (dbExtra, routingContext)
import           Database.Bolt.Connection.Pipe         (close, connectWithRouting)
import           Database.Bolt.Connection.RoutingTable
import           Database.Bolt.Connection.Type

import           Database.Bolt.Value.Helpers (isV5_6)
import           Database.Bolt.Value.Type    (Value)

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (Async, async, cancel)
import           Control.Concurrent.MVar   (MVar, modifyMVar, newEmptyMVar, newMVar, takeMVar,
                                            tryPutMVar, withMVar)
import           Control.Exception         (IOException, SomeAsyncException, SomeException,
                                            bracket, finally, fromException, onException,
                                            throwIO, tryJust)
import           Control.Monad             (forM_, when)
import           Control.Monad.Catch       (MonadMask)
import qualified Control.Monad.Catch       as MC (generalBracket, ExitCase(..))
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Containers.ListUtils (nubOrd)
import           Data.Default              (Default (..))
import           Data.List                 (partition, sortOn)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           Data.Time.Clock           (UTCTime (..), diffUTCTime, getCurrentTime)
import           GHC.Stack                 (HasCallStack)

-- | Connection pool for Neo4j cluster routing with least-connections selection.
data RouterPool = RouterPool
  { rpPoolCfg      :: RouterPoolCfg
  , rpRoutingCtx   :: Map Text Value
    -- ^ Routing context sent in HELLO and ROUTE requests, built from the
    -- initial 'BoltCfg' (contains @"address"@ key with @host:port@).
    -- Immutable for the lifetime of the pool.
  , rpState        :: MVar PoolState
  , rpRefreshLock  :: MVar ()
    -- ^ empty = available; full = a thread is refreshing the routing table
  , rpReaper       :: Async ()
  }

data PoolState = PoolState
  { psServers :: Map ServerAddress ServerPool
  , psTable   :: RoutingTable
  , psRRIndex :: !Int
    -- ^ incremented each acquire; rotates the server list before
    -- sorting by spInUse so equal-count servers are picked in
    -- round-robin order
  }

data ServerPool = ServerPool
  { spIdle  :: [IdlePipe]
    -- ^ idle pipes, most-recently-used first
  , spInUse :: !Int
    -- ^ count of checked-out pipes
  }

data IdlePipe = IdlePipe
  { ipPipe      :: !Pipe
  , ipIdleSince :: !UTCTime
  }

-- | Configuration for 'RouterPool'.
data RouterPoolCfg = RouterPoolCfg
  { rpcBoltCfg      :: BoltCfg
  , rpcIdleTimeout  :: Double
    -- ^ Idle timeout in seconds (default 30)
  , rpcMaxPerServer :: Int
    -- ^ Max connections per server (default 10, soft limit)
  , rpcReapInterval :: Int
    -- ^ Seconds between reaper runs (default 10)
  }

instance Default RouterPoolCfg where
  def = RouterPoolCfg
    { rpcBoltCfg      = def
    , rpcIdleTimeout  = 30
    , rpcMaxPerServer = 10
    , rpcReapInterval = 10
    }

emptyServerPool :: ServerPool
emptyServerPool = ServerPool { spIdle = [], spInUse = 0 }

-- | Connect to a Neo4j cluster and create a router pool.
connectRouterPool :: (MonadIO m, HasCallStack) => RouterPoolCfg -> m RouterPool
connectRouterPool poolCfg@RouterPoolCfg{..} = liftIO $ do
    let cfg = rpcBoltCfg
        routingCtx = routingContext cfg

    -- Bootstrap: get initial routing table
    bootstrapPipe <- connectWithRouting cfg (Just routingCtx)

    -- ROUTE message requires BOLT v4.3+; fail clearly if server negotiated older version.
    -- We check for 5.6+, because we do not support older versions anyway.
    when (not (isV5_6 (pipe_version bootstrapPipe))) $ do
      close bootstrapPipe
      throwIO $ RoutingError "hasbolt supports Router pool with BOLT v5.6+ but server negotiated an older version"

    rtResult <- runE bootstrapPipe $
                  sendRawRequest (RequestRoute routingCtx [] (dbExtra (database cfg)))

    -- Close the bootstrap pipe — the pool creates connections on demand
    close bootstrapPipe

    case rtResult of
      Left err -> throwIO err
      Right resp -> do
        now <- getCurrentTime
        case parseRoutingTable now (succMap resp) of
          Left msg -> throwIO (RoutingError msg)
          Right table -> do
            let allAddrs = nubOrd (rtReaders table <> rtWriters table <> rtRouters table)
                servers = M.fromList [(addr, emptyServerPool) | addr <- allAddrs]
                ps = PoolState { psServers = servers, psTable = table, psRRIndex = 0 }

            stVar <- newMVar ps
            refreshLock <- newEmptyMVar
            reaper <- async (reaperLoop poolCfg stVar)

            pure RouterPool
              { rpPoolCfg      = poolCfg
              , rpRoutingCtx   = routingCtx
              , rpState        = stVar
              , rpRefreshLock  = refreshLock
              , rpReaper       = reaper
              }

-- | Get the current routing table from the pool.
getRoutingTable :: MonadIO m => RouterPool -> m RoutingTable
getRoutingTable RouterPool{..} = liftIO $ withMVar rpState (pure . psTable)

-- | Close a router pool. Cancels the reaper thread and closes all idle pipes.
-- In-use pipes cannot be closed here; they will be closed when released.
closeRouterPool :: MonadIO m => RouterPool -> m ()
closeRouterPool RouterPool{..} = liftIO $ do
    cancel rpReaper
    let distantPast = UTCTime (toEnum 0) 0  -- 1858-11-17, always expired
        emptyPS = PoolState { psServers = M.empty
                            , psTable = RoutingTable [] [] [] 0 distantPast
                            , psRRIndex = 0
                            }

    old <- modifyMVar rpState $ \ps -> pure (emptyPS, ps)

    -- Close all idle pipes outside the lock
    forM_ (M.elems (psServers old)) $ \sp ->
      forM_ (spIdle sp) $ \ip ->
        close (ipPipe ip)

-- | Run a 'BoltActionT' on a writer pipe from the pool.
runRouterPool :: (MonadIO m, MonadMask m, HasCallStack) => RouterPool -> BoltActionT m a -> m a
runRouterPool rp action = do
    result <- runRouterPoolE rp action
    case result of
      Right x -> pure x
      Left e  -> liftIO $ throwIO e

-- | Run a 'BoltActionT' on a writer pipe, returning errors as 'Left'.
runRouterPoolE :: (MonadIO m, MonadMask m) => RouterPool -> BoltActionT m a -> m (Either BoltError a)
runRouterPoolE rp action = runPoolAction rp WriteMode action

-- | Run a 'BoltActionT' on a reader pipe from the pool.
runRouterPoolRead :: (MonadIO m, MonadMask m, HasCallStack) => RouterPool -> BoltActionT m a -> m a
runRouterPoolRead rp action = do
    result <- runRouterPoolReadE rp action
    case result of
      Right x -> pure x
      Left e  -> liftIO $ throwIO e

-- | Run a 'BoltActionT' on a reader pipe, returning errors as 'Left'.
runRouterPoolReadE :: (MonadIO m, MonadMask m) => RouterPool -> BoltActionT m a -> m (Either BoltError a)
runRouterPoolReadE rp action = runPoolAction rp ReadMode action

-- Internal helpers

-- | Run a BoltActionT with proper pipe lifecycle management.
-- On success or application error: pipe is returned to the pool.
-- On connection error or exception: pipe is destroyed.
runPoolAction :: (MonadIO m, MonadMask m) => RouterPool -> AccessMode -> BoltActionT m a -> m (Either BoltError a)
runPoolAction rp mode action = do
    (result, _) <- MC.generalBracket
      (liftIO $ acquirePipe rp mode)
      (\(pipe, addr) exitCase -> liftIO $ case exitCase of
          MC.ExitCaseSuccess (Right _) -> releasePipe rp addr pipe
          MC.ExitCaseSuccess (Left err)
            | isConnectionError err    -> destroyPipe rp addr pipe
            | otherwise                -> releasePipe rp addr pipe
          _                            -> destroyPipe rp addr pipe)
      (\(pipe, _) -> runE pipe action)
    pure result

-- | Acquire a pipe using least-connections selection with round-robin tie-breaking.
--
-- 1. Refresh the routing table if expired (see 'maybeRefresh').
-- 2. Under rpState: pick the best server (fewest in-use connections, rotated by
--    round-robin index). If it has an idle pipe, take it; otherwise reserve a
--    slot (bump spInUse) and release the lock.
-- 3. Outside the lock: connect to the best server. On failure, fall back through
--    the ranked list, adjusting counters under rpState between attempts.
acquirePipe :: RouterPool -> AccessMode -> IO (Pipe, ServerAddress)
acquirePipe rp@RouterPool{..} mode = do
    maybeRefresh rp

    result <- modifyMVar rpState $ \ps -> do
      let eligible = case mode of
            WriteMode -> rtWriters (psTable ps)
            ReadMode  -> rtReaders (psTable ps)

      when (null eligible) $
        throwIO $ NoServersAvailable mode

      -- Rotate eligible list by round-robin index before sorting, so that
      -- servers with equal spInUse are visited in round-robin order.
      let n = length eligible
          idx = psRRIndex ps `mod` max 1 n
          rotated = drop idx eligible <> take idx eligible
          ranked = sortOn (\addr -> maybe 0 spInUse (M.lookup addr (psServers ps))) rotated
          ps1 = ps { psRRIndex = psRRIndex ps + 1 }

      pickFromRanked ps1 ranked

    case result of
      Right (pipe, addr) -> pure (pipe, addr)
      Left addrs -> connectWithFallback addrs
  where
    -- Under the lock: try to grab an idle pipe from the best server.
    -- If none available, reserve a slot (bump spInUse) and return the ranked
    -- address list so the caller can connect outside the lock with fallback.
    pickFromRanked ps ranked =
      case ranked of
        [] -> throwIO $ NoServersAvailable mode
        (best:_) ->
          case M.lookup best (psServers ps) of
            Just sp | (ip:rest) <- spIdle sp -> do
              -- Found an idle pipe on the best server
              let sp' = sp { spIdle = rest, spInUse = spInUse sp + 1 }
                  ps' = ps { psServers = M.insert best sp' (psServers ps) }

              pure (ps', Right (ipPipe ip, best))
            _ -> do
              -- No idle pipe - reserve a slot on the best server, return full
              -- ranked list so caller can fall back to others on connect failure.
              let sp = M.findWithDefault emptyServerPool best (psServers ps)
                  sp' = sp { spInUse = spInUse sp + 1 }
                  ps' = ps { psServers = M.insert best sp' (psServers ps) }

              pure (ps', Left ranked)

    -- Connect outside the lock, falling back through ranked servers.
    -- The first server already has its spInUse bumped from pickFromRanked.
    -- On failure: decrement the failed server's count and bump the next one.
    connectWithFallback [] = throwIO $ NoServersAvailable mode
    connectWithFallback [addr] = do
      -- Last server: no fallback, just throw on failure (slot already reserved)
      pipe <- openSinglePipeIO (rpcBoltCfg rpPoolCfg) rpRoutingCtx addr
                `onException` decrementInUse addr
      pure (pipe, addr)
    connectWithFallback (addr:rest@(next:_)) = do
      connectResult <- tryJust syncException (openSinglePipeIO (rpcBoltCfg rpPoolCfg) rpRoutingCtx addr)
      case connectResult of
        Right pipe -> pure (pipe, addr)
        Left _ -> do
          -- Shift the reservation: decrement failed, bump next
          modifyMVar rpState $ \ps -> do
            let ps' = adjustServer addr decrementSP $
                      adjustServer next bumpSP ps
            pure (ps', ())
          connectWithFallback rest

    decrementInUse addr =
      modifyMVar rpState $ \ps -> do
        let ps' = adjustServer addr decrementSP ps
        pure (ps', ())

    adjustServer addr f ps =
      ps { psServers = M.adjust f addr (psServers ps) }

    decrementSP sp = sp { spInUse = max 0 (spInUse sp - 1) }
    bumpSP sp = sp { spInUse = spInUse sp + 1 }

-- | Return a pipe to the idle list (or close it if excess).
releasePipe :: RouterPool -> ServerAddress -> Pipe -> IO ()
releasePipe RouterPool{..} addr pipe = do
    now <- getCurrentTime
    excess <- modifyMVar rpState $ \ps -> do
      case M.lookup addr (psServers ps) of
        Nothing -> do
          -- Server was removed from routing table; close the pipe
          pure (ps, True)
        Just sp -> do
          let inUse' = max 0 (spInUse sp - 1)
              totalAfter = length (spIdle sp) + 1 + inUse'
          if totalAfter > rpcMaxPerServer rpPoolCfg
            then do
              -- Excess pipe - just decrement inUse, mark for close
              let sp' = sp { spInUse = inUse' }
              pure (ps { psServers = M.insert addr sp' (psServers ps) }, True)
            else do
              -- Return to idle list (MRU order: prepend)
              let ip = IdlePipe { ipPipe = pipe, ipIdleSince = now }
                  sp' = sp { spIdle = ip : spIdle sp, spInUse = inUse' }
              pure (ps { psServers = M.insert addr sp' (psServers ps) }, False)
    when excess $ close pipe

-- | Destroy a broken pipe (decrement inUse, close outside lock).
destroyPipe :: RouterPool -> ServerAddress -> Pipe -> IO ()
destroyPipe RouterPool{..} addr pipe = do
    modifyMVar rpState $ \ps -> do
      let servers' = M.adjust (\sp -> sp { spInUse = max 0 (spInUse sp - 1) }) addr (psServers ps)
      pure (ps { psServers = servers' }, ())
    close pipe

-- | Background reaper thread that closes idle pipes past the timeout.
reaperLoop :: RouterPoolCfg -> MVar PoolState -> IO ()
reaperLoop RouterPoolCfg{..} stVar = go
  where
    go = do
      threadDelay (rpcReapInterval * 1000000)
      _ <- tryJust syncException reapOnce
      go

    reapOnce = do
      expired <- modifyMVar stVar $ \ps -> do
        now <- getCurrentTime
        let cutoff = realToFrac rpcIdleTimeout
            (collected, servers') = M.mapAccumWithKey (reapServer now cutoff) [] (psServers ps)
        pure (ps { psServers = servers' }, collected)

      -- Close expired pipes outside the lock
      forM_ expired $ \ip -> close (ipPipe ip)

    reapServer now cutoff acc _addr sp =
      let (keep, expired) = partition (\ip -> diffUTCTime now (ipIdleSince ip) < cutoff) (spIdle sp)
      in (acc ++ expired, sp { spIdle = keep })

-- | Refresh the routing table if its TTL has expired.
--
-- Concurrency design:
--   * rpRefreshLock (MVar): at most one thread fetches a new table at a time.
--     Other threads that see an expired table will skip the refresh and proceed
--     with the current (stale) table — the refreshing thread's update will be
--     visible on their next acquirePipe call.
--   * rpState (MVar): held only briefly for reads and reconciliation, never
--     during network I/O, so other threads can continue acquiring pipes.
maybeRefresh :: RouterPool -> IO ()
maybeRefresh RouterPool{..} = do
    -- Check TTL without blocking other threads for long
    now <- getCurrentTime
    needsRefresh <- withMVar rpState $ \ps ->
      pure (isExpired now (psTable ps))

    when needsRefresh $ do
      -- Non-blocking: if another thread is already refreshing, we skip rather
      -- than wait. The stale table is still usable (servers don't vanish
      -- instantly), and blocking all acquirePipe callers on a network fetch
      -- would add latency for no benefit. The refreshed table will be picked
      -- up on subsequent calls.
      acquired <- tryPutMVar rpRefreshLock ()
      when acquired $ flip finally (takeMVar rpRefreshLock) $ do
        routers <- withMVar rpState $ \ps -> pure (rtRouters (psTable ps))
        -- Network I/O — no locks held, other threads freely acquire pipes
        mbNewTable <- tryJust syncException (fetchRoutingTable (rpcBoltCfg rpPoolCfg) rpRoutingCtx routers)
        case mbNewTable of
          Left _ -> pure ()  -- sync failure: proceed with stale table
          Right newTable -> do
            pipesToClose <- modifyMVar rpState $ \ps -> do
              -- Re-check expiry: another thread may have refreshed while we
              -- were fetching (shouldn't happen with the lock, but defensive)
              now' <- getCurrentTime
              if not (isExpired now' (psTable ps))
                then pure (ps, [])
                else do
                  let (ps', removed) = reconcileState ps newTable
                  pure (ps', removed)
            -- Close removed pipes outside the lock
            forM_ pipesToClose $ \ip -> close (ipPipe ip)

-- | Reconcile pool state with a new routing table.
-- Returns updated state and list of idle pipes from removed servers to close.
reconcileState :: PoolState -> RoutingTable -> (PoolState, [IdlePipe])
reconcileState ps newTable =
    let newAddrList = nubOrd (rtReaders newTable <> rtWriters newTable <> rtRouters newTable)
        newAddrSet = M.fromList [(a, ()) | a <- newAddrList]
        keptServers = M.intersectionWith (\sp _ -> sp) (psServers ps) newAddrSet
        newServers = M.fromList [(a, emptyServerPool) | a <- newAddrList, not (M.member a (psServers ps))]
        removedServers = M.difference (psServers ps) newAddrSet
        removedPipes = concatMap spIdle (M.elems removedServers)

        ps' = PoolState
          { psServers = M.union keptServers newServers
          , psTable   = newTable
          , psRRIndex = psRRIndex ps
          }

    in (ps', removedPipes)

-- | Fetch a new routing table by trying each router in sequence.
-- Each attempt opens an ephemeral pipe, sends RequestRoute, and closes the pipe.
-- No pool locks are held — this is pure network I/O.
fetchRoutingTable :: BoltCfg -> Map Text Value -> [ServerAddress] -> IO RoutingTable
fetchRoutingTable cfg routingCtx routers = tryRouters routers
  where
    tryRouters [] = throwIO RoutingTableUnavailable
    tryRouters (addr:rest) = do
      result <- tryJust syncException $
        bracket (openSinglePipeIO cfg routingCtx addr) close $ \pipe ->
          runE pipe $ sendRawRequest (RequestRoute routingCtx [] (dbExtra (database cfg)))
      case result of
        Left _ -> tryRouters rest
        Right (Left _) -> tryRouters rest
        Right (Right resp) -> do
          now <- getCurrentTime
          case parseRoutingTable now (succMap resp) of
            Left _ -> tryRouters rest
            Right newTable -> pure newTable

openSinglePipeIO :: BoltCfg -> Map Text Value -> ServerAddress -> IO Pipe
openSinglePipeIO cfg routingCtx ServerAddress{..} =
    connectWithRouting (cfg { host = serverHost, port = serverPort }) (Just routingCtx)

isConnectionError :: BoltError -> Bool
isConnectionError CannotReadChunk     = True
isConnectionError TimeOut             = True
isConnectionError (NonHasboltError e) = case fromException e of
  Just (_ :: IOException) -> True
  Nothing                 -> False
isConnectionError _                   = False

-- | 'tryJust' filter that lets async exceptions propagate and catches everything else.
syncException :: SomeException -> Maybe SomeException
syncException e = case fromException e of
  Just (_ :: SomeAsyncException) -> Nothing
  Nothing                        -> Just e
