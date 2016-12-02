{-# LANGUAGE FlexibleInstances #-}

module Database.Bolt.Internal.PackStream
    ( PackStream (..)
    , encodeStrict
    ) where

import           Data.Binary
import           Data.Binary.IEEE754
import           Data.ByteString               (ByteString, append, cons,
                                                singleton)
import qualified Data.ByteString               as B (concat, length, pack)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Map.Strict               (Map (..), assocs, size)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (encodeUtf8)
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Codes
import           GHC.Float                     (float2Double)

class PackStream a where
  pack :: a -> ByteString

instance PackStream () where
  pack () = singleton nullCode

instance PackStream Bool where
  pack False = singleton falseCode
  pack True  = singleton trueCode

instance PackStream Int where
  pack int | isTinyInt int = encodeStrict (fromIntegral int :: Word8)
           | isIntX  8 int = cons  int8Code $ encodeStrict (fromIntegral int :: Word8)
           | isIntX 16 int = cons int16Code $ encodeStrict (fromIntegral int :: Word16)
           | isIntX 32 int = cons int32Code $ encodeStrict (fromIntegral int :: Word32)
           | isIntX 64 int = cons int64Code $ encodeStrict (fromIntegral int :: Word64)

instance PackStream Integer where
  pack int | isIntX 64 int = pack (fromIntegral int :: Int)
           | otherwise     = error "Cannot pack so large Integer"

instance PackStream Double where
  pack dbl = cons doubleCode $ encodeStrict (doubleToWord dbl)

instance PackStream Float where
  pack flt = cons doubleCode $ encodeStrict (doubleToWord (float2Double flt))

instance PackStream Text where
  pack text = mkPackedCollection (B.length pbs) pbs (textConst, text8Code, text16Code, text32Code)
    where pbs = encodeUtf8 text

instance PackStream a => PackStream [a] where
  pack lst = mkPackedCollection (length lst) pbs (listConst, list8Code, list16Code, list32Code)
    where pbs = B.concat $ map pack lst

instance PackStream a => PackStream (Map Text a) where
  pack dict = mkPackedCollection (size dict) pbs (dictConst, dict8Code, dict16Code, dict32Code)
    where pbs = B.concat $ map mkPairPack $ assocs dict
          mkPairPack (key, val) = pack key `append` pack val

encodeStrict :: Binary a => a -> ByteString
encodeStrict = toStrict . encode

mkPackedCollection :: Int -> ByteString -> (Word8, Word8, Word8, Word8) -> ByteString
mkPackedCollection len bst (wt, w8, w16, w32) = helper len
  where helper len | len < 2^4  = cons (wt + fromIntegral len) bst
                   | len < 2^8  = cons w8 $ cons (fromIntegral len) bst
                   | len < 2^16 = cons w16 $ encodeStrict (fromIntegral len :: Word16) `append` bst
                   | len < 2^32 = cons w32 $ encodeStrict (fromIntegral len :: Word32) `append` bst
                   | otherwise  = error "Cannot pack so large collection"
