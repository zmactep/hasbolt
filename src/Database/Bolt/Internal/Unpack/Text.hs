module Database.Bolt.Internal.Unpack.Text
    ( UnpackStream (..)
    ) where

import           Control.Monad.Trans.State         (gets, modify)
import qualified Data.ByteString                   as B (drop, take)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.Word                         (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.UnpackStream

instance UnpackStream Text where
  unpack = unpackText

unpackText :: Monad m => UnpackT m Text
unpackText = unpackW8 >>= unpackTextByMarker

unpackTextByMarker :: Monad m => Word8 -> UnpackT m Text
unpackTextByMarker m | isTinyText m    = unpackTextBySize (getSize m)
                     | m == text8Code  = convertToInt <$> unpackW8 >>= unpackTextBySize
                     | m == text16Code = convertToInt <$> unpackW16 >>= unpackTextBySize
                     | m == text32Code = convertToInt <$> unpackW32 >>= unpackTextBySize
                     | otherwise       = fail "Not a Text value"

unpackTextBySize :: Monad m => Int -> UnpackT m Text
unpackTextBySize size = do str <- gets (B.take size)
                           modify (B.drop size)
                           return $ decodeUtf8 str
