{-# LANGUAGE FlexibleInstances #-}

module Database.Bolt.Internal.Unpack.Dict
    ( UnpackStream (..)
    ) where

import           Control.Monad                     (forM)
import           Data.Map.Strict                   (Map (..))
import qualified Data.Map.Strict                   as M (fromList)
import           Data.Text                         (Text)
import           Data.Word                         (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.Text
import           Database.Bolt.Internal.Unpack.UnpackStream

instance UnpackStream a => UnpackStream (Map Text a) where
  unpack = unpackDict

unpackDict :: (Monad m, UnpackStream a) => UnpackST m (Map Text a)
unpackDict = unpackW8 >>= unpackDictByMarker

unpackDictByMarker :: (Monad m, UnpackStream a) => Word8 -> UnpackST m (Map Text a)
unpackDictByMarker m | isTinyDict m    = unpackDictBySize (getSize m)
                     | m == dict8Code  = convertToInt <$> unpackW8 >>= unpackDictBySize
                     | m == dict16Code = convertToInt <$> unpackW16 >>= unpackDictBySize
                     | m == dict32Code = convertToInt <$> unpackW32 >>= unpackDictBySize
                     | otherwise       = error "Not a Dict value"

unpackDictBySize :: (Monad m, UnpackStream a) => Int -> UnpackST m (Map Text a)
unpackDictBySize = (M.fromList <$>) . unpackPairsBySize

unpackPairsBySize :: (Monad m, UnpackStream a) => Int -> UnpackST m [(Text, a)]
unpackPairsBySize size = forM [1..size] $ const $ do
                           key <- unpack
                           value <- unpack
                           return (key, value)
