module Database.Bolt
    ( BoltActionT (..)
    , connect, close
    , Pipe
    , BoltCfg (..), Default (..)
    , Value (..), BoltValue (..)
    ) where

import           Database.Bolt.Connection.Pipe
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Instances
import           Database.Bolt.Value.Type

import           Control.Monad.Trans.Reader    (ReaderT (..))
import           Data.Default                  (Default (..))

type BoltActionT = ReaderT Pipe
