module Database.Bolt.Protocol
    ( BoltCfg (..)
    , Default (..)
    ) where

import qualified Data.ByteString                            as B (pack)
import           Data.Default                               (Default (..))
import qualified Data.Text                                  as T (pack)
import           Database.Bolt.Internal.Protocol.Types

instance Default BoltCfg where
  def = BoltCfg { magic         = B.pack [96, 96, 176, 23]
                , version       = 1
                , userAgent     = T.pack "hasbot/1.0"
                , maxChunkSize  = 65535
                , socketTimeout = 5
                , host          = "127.0.0.1"
                , port          = 7474
                , user          = T.pack ""
                , password      = T.pack ""
                }
