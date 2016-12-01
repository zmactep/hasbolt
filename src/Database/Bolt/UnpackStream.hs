module Database.Bolt.UnpackStream
    ( UnpackStream (..), UnpackST (..)
    , Unpacked (..)
    , unpackAll
    ) where

import           Control.Applicative                        ((<$>))
import           Data.Binary                                (decode)
import           Data.ByteString.Lazy                       (fromStrict)
import           Data.Map.Strict                            (Map (..))
import           Data.Text                                  (Text)
import           Data.Word                                  (Word8)

import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Unpack.Dict
import           Database.Bolt.Internal.Unpack.List
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.Primitive
import           Database.Bolt.Internal.Unpack.Text
import           Database.Bolt.Internal.Unpack.UnpackStream

data Unpacked = N ()
              | B Bool
              | I Int
              | F Double
              | T Text
              | L [Unpacked]
              | M (Map Text Unpacked)

instance UnpackStream Unpacked where
  unpack = firstByte >>= unpackByFirstByte

unpackByFirstByte :: Monad m => Word8 -> UnpackST m Unpacked
unpackByFirstByte w | isNull   w = N <$> unpack
                    | isBool   w = B <$> unpack
                    | isInt    w = I <$> unpack
                    | isDouble w = F <$> unpack
                    | isText   w = T <$> unpack
                    | isList   w = L <$> unpack
                    | isDict   w = M <$> unpack
                    | otherwise  = fail "Not recognisable type"

firstByte :: Monad m => UnpackST m Word8
firstByte = do w <- topBS 1
               return $ decode (fromStrict w)
