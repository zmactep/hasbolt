module Database.Bolt.Internal.Protocol.Request
    ( AuthToken (..)
    , Request (..)
    , PackStream (..)
    , initByConfig
    ) where

import           Data.ByteString                       (ByteString, append,
                                                        cons)
import qualified Data.ByteString                       as B (concat)
import qualified Data.Map.Strict                       as M (fromList)
import           Data.Text                             (Text)
import qualified Data.Text                             as T (pack)
import           Data.Word                             (Word8, Word16)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.PackStream     (PackStream (..),
                                                        encodeStrict)
import           Database.Bolt.Internal.Protocol.Types (BoltCfg (..))

data AuthToken = AuthToken { scheme      :: Text
                           , principal   :: Text
                           , credentials :: Text
                           }

data Request = RequestInit { agent :: Text
                           , token :: AuthToken
                           }
             | RequestRun  { statement  :: Text
                           , parameters :: Text
                           }
             | RequestAckFailure
             | RequestReset
             | RequestDiscardAll
             | RequestPullAll

instance PackStream AuthToken where
  pack token = pack $ M.fromList [ (T.pack "scheme", scheme token)
                                 , (T.pack "principal", principal token)
                                 , (T.pack "credentials", credentials token)
                                 ]

instance PackStream Request where
  pack (RequestInit userAgent authToken) = packRequest 1 [pack userAgent, pack authToken]
  pack (RequestRun statement parameters) = packRequest 16 []
  pack RequestAckFailure                 = packRequest 14 []
  pack RequestReset                      = packRequest 15 []
  pack RequestDiscardAll                 = packRequest 47 []
  pack RequestPullAll                    = packRequest 63 []

initByConfig :: BoltCfg -> Request
initByConfig bcfg = RequestInit (userAgent bcfg) auth
  where auth = AuthToken (T.pack "basic") (user bcfg) (password bcfg)

packRequest :: Word8 -> [ByteString] -> ByteString
packRequest code pbss | len < 16   = (structConst + fromIntegral len) `cons` sigData
                      | len < 2^8  = struct8Code `cons` fromIntegral len `cons` sigData
                      | len < 2^16 = struct16Code `cons` encodeStrict len `append` sigData
  where len = fromIntegral $ length pbss :: Word16
        sigData = encodeStrict code `append` B.concat pbss
