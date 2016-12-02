{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol
    ( BoltCfg (..)
    , Default (..)
    , Pipe
    , connect, close
    ) where

import           Data.Default                               (Default (..))
import           Database.Bolt.Internal.Protocol.Types
import           Database.Bolt.Internal.Protocol.Connection

instance Default BoltCfg where
  def = BoltCfg { magic         = 1616949271
                , version       = 1
                , userAgent     = "hasbolt/1.0"
                , maxChunkSize  = 65535
                , socketTimeout = 5
                , host          = "127.0.0.1"
                , port          = 7687
                , user          = ""
                , password      = ""
                }
