module Database.Bolt.Internal.Protocol.Types
    ( BoltCfg (..)
    , Pipe (..)
    ) where

import           Control.Monad.Trans.State
import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           Data.Word                 (Word16, Word32)
import           Network.Simple.TCP        (SockAddr, Socket)

data BoltCfg = BoltCfg { magic         :: Word32
                       , version       :: Word32
                       , userAgent     :: Text
                       , maxChunkSize  :: Word16
                       , socketTimeout :: Int
                       , host          :: String
                       , port          :: Int
                       , user          :: Text
                       , password      :: Text
                       }

data Pipe = Pipe { connectionSocket :: Socket
                 , mcs              :: Word16
                 }
