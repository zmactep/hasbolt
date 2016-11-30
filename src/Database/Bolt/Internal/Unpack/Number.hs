module Database.Bolt.Internal.Unpack.Number
    ( UnpackStream (..)
    , unpackW8, unpackW16, unpackW32, unpackW64
    , unpackI8, unpackI16, unpackI32, unpackI64
    , unpackInt, unpackDouble
    ) where

import           Data.Binary                       (Binary (..), decode)
import           Data.Binary.IEEE754               (wordToDouble)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Int                          (Int16, Int32, Int64, Int8)
import           Data.Word                         (Word16, Word32, Word64,
                                                    Word8)
import           GHC.Float                     (double2Float)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.UnpackStream

instance UnpackStream Int where
  unpack = unpackInt

instance UnpackStream Integer where
  unpack = fromIntegral <$> unpackInt

instance UnpackStream Double where
  unpack = unpackDouble

instance UnpackStream Float where
  unpack = double2Float <$> unpackDouble

unpackInt :: Monad m => UnpackST m Int
unpackInt = unpackW8 >>= unpackIntByMarker

unpackIntByMarker :: Monad m => Word8 -> UnpackST m Int
unpackIntByMarker m | isTinyWord m   = return . convertToInt $ (fromIntegral m :: Int8)
                    | m == int8Code  = convertToInt <$> unpackI8
                    | m == int16Code = convertToInt <$> unpackI16
                    | m == int32Code = convertToInt <$> unpackI32
                    | m == int64Code = convertToInt <$> unpackI64
                    | otherwise      = fail "Not an Int value"

unpackDouble :: Monad m => UnpackST m Double
unpackDouble = unpackW8 >>= unpackDoubleByMarker

unpackDoubleByMarker :: Monad m => Word8 -> UnpackST m Double
unpackDoubleByMarker m | m == doubleCode = wordToDouble <$> unpackW64
                       | otherwise       = fail "Not a Double value"

unpackNum :: (Monad m, Binary a, Num a) => Int -> UnpackST m a
unpackNum = (decode . fromStrict <$>) . getBS

unpackW8 :: Monad m => UnpackST m Word8
unpackW8 = unpackNum 1

unpackW16 :: Monad m => UnpackST m Word16
unpackW16 = unpackNum 2

unpackW32 :: Monad m => UnpackST m Word32
unpackW32 = unpackNum 4

unpackW64 :: Monad m => UnpackST m Word64
unpackW64 = unpackNum 8

unpackI8 :: Monad m => UnpackST m Int8
unpackI8 = unpackNum 1

unpackI16 :: Monad m => UnpackST m Int16
unpackI16 = unpackNum 2

unpackI32 :: Monad m => UnpackST m Int32
unpackI32 = unpackNum 4

unpackI64 :: Monad m => UnpackST m Int64
unpackI64 = unpackNum 8
