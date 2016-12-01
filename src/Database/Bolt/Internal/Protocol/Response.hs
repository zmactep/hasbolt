module Database.Bolt.Internal.Protocol.Response
    ( Response (..)
    , UnpackStream (..)
    ) where

import           Database.Bolt.UnpackStream (UnpackT (..), UnpackStream (..),
                                             Unpacked)

data Response = ResponseSuccess Unpacked
              | ResponseRecord Unpacked
              | ResponseIgnored Unpacked
              | ResponseFailure Unpacked

instance UnpackStream Response where
  unpack = unpack >>= unpackByFirstByte

unpackByFirstByte :: Monad m => Int -> UnpackT m Response
unpackByFirstByte w | w == 112  = ResponseSuccess <$> unpack
                    | w == 113  = ResponseRecord  <$> unpack
                    | w == 126  = ResponseIgnored <$> unpack
                    | w == 127  = ResponseFailure <$> unpack
                    | otherwise = fail "Not recognisable response"
