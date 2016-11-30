module Database.Bolt.Internal.Unpack.List
    ( UnpackStream (..)
    ) where

import           Control.Monad                     (forM)
import           Data.Word                         (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.UnpackStream

instance UnpackStream a => UnpackStream [a] where
  unpack = unpackList

unpackList :: (Monad m, UnpackStream a) => UnpackST m [a]
unpackList = unpackW8 >>= unpackListByMarker

unpackListByMarker :: (Monad m, UnpackStream a) => Word8 -> UnpackST m [a]
unpackListByMarker m | isTinyList m    = unpackListBySize (getSize m)
                     | m == list8Code  = convertToInt <$> unpackW8 >>= unpackListBySize
                     | m == list16Code = convertToInt <$> unpackW16 >>= unpackListBySize
                     | m == list32Code = convertToInt <$> unpackW32 >>= unpackListBySize
                     | otherwise       = fail "Not a List value"

unpackListBySize :: (Monad m, UnpackStream a) => Int -> UnpackST m [a]
unpackListBySize size = forM [1..size] $ const unpack
