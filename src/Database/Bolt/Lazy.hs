{- | Lazy API

This module exposes various query functions that use 'System.IO.Unsafe.unsafeInterleaveIO'
to make fetching result lazy.

When using them, do not forget to read all the records before you send a next query.

__Important__: this is not compatible with t'Database.Bolt.RouterPool'.
-}
module Database.Bolt.Lazy
    ( BoltActionT
    , BoltError (..), UnpackError (..)
    , connect, close, reset
    , run, runE, queryP, query, queryP_, query_
    , transact, transactRead
    , (=:), props
    , Pipe
    , BoltCfg (..)
    , Value (..), IsValue (..), Structure (..), Record, RecordValue (..), exact, exactMaybe, at
    , Node (..), Relationship (..), URelationship (..), Path (..)
    ) where

import           Database.Bolt.Connection
import           Database.Bolt hiding (query, queryP)
