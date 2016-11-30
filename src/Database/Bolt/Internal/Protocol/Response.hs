module Database.Bolt.Internal.Protocol.Response
    ( Response (..)
    , PackStream (..)
    , UnpackStream (..)
    ) where

import           Data.ByteString            (append)
import qualified Data.Map                   as M (fromList)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack)
import           Database.Bolt.PackStream   (PackStream (..))
import           Database.Bolt.UnpackStream (UnpackStream (..))

data Response = ResponseSuccess
              | ResponseRecord
              | ResponseIgnored
              | ResponseFailure

instance PackStream Response where
  pack ResponseSuccess = pack (112::Int)
  pack ResponseRecord  = pack (113::Int)
  pack ResponseIgnored = pack (126::Int)
  pack ResponseFailure = pack (127::Int)
