module Database.Bolt.Internal.Protocol.Request
    ( AuthToken (..)
    , Request (..)
    , PackStream (..)
    , initByConfig
    ) where

import           Data.ByteString                       (append)
import qualified Data.Map.Strict                       as M (fromList)
import           Data.Text                             (Text)
import qualified Data.Text                             as T (pack)
import           Database.Bolt.Internal.Protocol.Types (BoltCfg (..))
import           Database.Bolt.PackStream              (PackStream (..))

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
  pack at = pack $ M.fromList [ (T.pack "scheme", scheme at)
                              , (T.pack "principal", principal at)
                              , (T.pack "credentials", credentials at)
                              ]

instance PackStream Request where
  pack (RequestInit userAgent authToken) = pack (1::Int) `append` pack userAgent `append` pack authToken
  pack (RequestRun statement parameters) = pack (16::Int)
  pack RequestAckFailure                 = pack (14::Int)
  pack RequestReset                      = pack (15::Int)
  pack RequestDiscardAll                 = pack (47::Int)
  pack RequestPullAll                    = pack (63::Int)

initByConfig :: BoltCfg -> Request
initByConfig bcfg = RequestInit (userAgent bcfg) auth
  where auth = AuthToken (T.pack "basic") (user bcfg) (password bcfg)
