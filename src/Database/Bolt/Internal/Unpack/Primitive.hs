module Database.Bolt.Internal.Unpack.Primitive
    ( UnpackStream (..)
    ) where

import           Data.Word                         (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.UnpackStream

instance UnpackStream () where
  unpack = unpackNull

instance UnpackStream Bool where
  unpack = unpackBool

unpackBool :: Monad m => UnpackT m Bool
unpackBool = unpackW8 >>= unpackBoolByMarker

unpackNull :: Monad m => UnpackT m ()
unpackNull = unpackW8 >>= unpackNullByMarker

unpackBoolByMarker :: Monad m => Word8 -> UnpackT m Bool
unpackBoolByMarker m | m == falseCode = return False
                     | m == trueCode  = return True
                     | otherwise      = fail "Not a Bool value"

unpackNullByMarker :: Monad m => Word8 -> UnpackT m ()
unpackNullByMarker m | m == nullCode = return ()
                     | otherwise     = fail "Not a Null value"
