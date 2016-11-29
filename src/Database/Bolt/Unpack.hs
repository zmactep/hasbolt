module Database.Bolt.Unpack
    ( unpack
    , unpackText
    , unpackW8, unpackW16, unpackW32, unpackW64
    , unpackI8, unpackI16, unpackI32, unpackI64
    ) where

import           Control.Applicative           ((<$>))
import           Control.Monad.Trans.State
import           Data.Binary
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B (drop, take)
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Int                      (Int16, Int32, Int64, Int8)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (decodeUtf8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.PackStream      (PS (..), PSObj (..),
                                                PackStream (..))

type UnpackST m a = StateT ByteString m a

unpack :: Monad m => ByteString -> m PS
unpack bs = fst <$> runStateT unpackAll bs

unpackAll :: Monad m => UnpackST m PS
unpackAll = do marker <- unpackW8
               return undefined

unpackByMarker :: Monad m => Word8 -> UnpackST m PS
unpackByMarker m | m == nullCode   = return $ PS ()                   -- Null
                 | m == falseCode  = return $ PS False                -- Bool
                 | m == trueCode   = return $ PS True
                 | isTinyWord m    = return . PS . convertToInt $ (fromIntegral m :: Int8)
                 | m == int8Code   = PS . convertToInt <$> unpackI8   -- Int
                 | m == int16Code  = PS . convertToInt <$> unpackI16
                 | m == int32Code  = PS . convertToInt <$> unpackI32
                 | m == int64Code  = PS . convertToInt <$> unpackI64
                 | m == doubleCode = PS <$> unpackF64                 -- Double
                 | isTinyText m    = PS <$> unpackText (getSize m)    -- Text
                 | m == text8Code  = PS <$> (convertToInt <$> unpackW8 >>= unpackText)
                 | m == text16Code = PS <$> (convertToInt <$> unpackW16 >>= unpackText)
                 | m == text32Code = PS <$> (convertToInt <$> unpackW32 >>= unpackText)
                 | isTinyList m    = PS <$> unpackList (getSize m)    -- List
                 | m == list8Code  = PS <$> (convertToInt <$> unpackW8 >>= unpackList)
                 | m == list16Code = PS <$> (convertToInt <$> unpackW16 >>= unpackList)
                 | m == list32Code = PS <$> (convertToInt <$> unpackW32 >>= unpackList)
                 | isTinyDict m    = PS <$> unpackDict (getSize m)    -- Dict
                 | m == dict8Code  = PS <$> (convertToInt <$> unpackW8 >>= unpackDict)
                 | m == dict16Code = PS <$> (convertToInt <$> unpackW16 >>= unpackDict)
                 | m == dict32Code = PS <$> (convertToInt <$> unpackW32 >>= unpackDict)

unpackList :: Monad m => Int -> UnpackST m [PS]
unpackList size = undefined

unpackDict :: Monad m => Int -> UnpackST m PSObj
unpackDict size = undefined

unpackText :: Monad m => Int -> UnpackST m Text
unpackText size = do str <- gets (B.take size)
                     modify (B.drop size)
                     return $ decodeUtf8 str

unpackX :: (Monad m, Binary a) => Int -> UnpackST m a
unpackX size = do str <- gets (B.take size)
                  modify (B.drop size)
                  return $ decode $ fromStrict str

unpackW8 :: Monad m => UnpackST m Word8
unpackW8 = unpackX 1

unpackW16 :: Monad m => UnpackST m Word16
unpackW16 = unpackX 2

unpackW32 :: Monad m => UnpackST m Word32
unpackW32 = unpackX 4

unpackW64 :: Monad m => UnpackST m Word64
unpackW64 = unpackX 8

unpackI8 :: Monad m => UnpackST m Int8
unpackI8 = unpackX 1

unpackI16 :: Monad m => UnpackST m Int16
unpackI16 = unpackX 2

unpackI32 :: Monad m => UnpackST m Int32
unpackI32 = unpackX 4

unpackI64 :: Monad m => UnpackST m Int64
unpackI64 = unpackX 8

unpackF64 :: Monad m => UnpackST m Double
unpackF64 = wordToDouble <$> unpackW64

getSize :: Word8 -> Int
getSize x = fromIntegral $ x .&. 15
