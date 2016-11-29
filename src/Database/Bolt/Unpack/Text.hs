module Database.Bolt.Unpack.Text
    ( UnpackStream (..)
    ) where

import           Control.Monad.Trans.State         (gets, modify)
import qualified Data.ByteString                   as B (drop, take)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.Word                         (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Unpack.Number
import           Database.Bolt.Unpack.UnpackStream

instance UnpackStream Text where
  unpack = unpackText

unpackText :: Monad m => UnpackST m Text
unpackText = unpackW8 >>= unpackTextByMarker

unpackTextByMarker :: Monad m => Word8 -> UnpackST m Text
unpackTextByMarker m | isTinyText m    = unpackTextBySize (getSize m)
                     | m == text8Code  = convertToInt <$> unpackW8 >>= unpackTextBySize
                     | m == text16Code = convertToInt <$> unpackW16 >>= unpackTextBySize
                     | m == text32Code = convertToInt <$> unpackW32 >>= unpackTextBySize
                     | otherwise       = fail "Not a Text value"

unpackTextBySize :: Monad m => Int -> UnpackST m Text
unpackTextBySize size = do str <- gets (B.take size)
                           modify (B.drop size)
                           return $ decodeUtf8 str
