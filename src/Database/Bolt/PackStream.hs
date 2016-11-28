{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Database.Bolt.PackStream
    ( PackStream (..)
    , PS (..), PSObj (..)
    ) where

import           Data.Binary
import           Data.Binary.IEEE754
import           Data.ByteString      (ByteString, append, cons, singleton)
import qualified Data.ByteString      as B (concat, length, pack)
import           Data.ByteString.Lazy (toStrict)
import           Data.Map.Strict      (Map (..), assocs, size)
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           GHC.Float            (float2Double)

class PackStream a where
  pack :: a -> ByteString

data PS = forall a. PackStream a => PS a
type PSObj = Map ByteString PS

instance PackStream () where
  pack () = singleton 192

instance PackStream Bool where
  pack False = singleton 194
  pack True  = singleton 195

instance PackStream Int where
  pack int | isTinyInt int = encodeStrict (fromIntegral int :: Word8)
           | isIntX  8 int = cons 200 $ encodeStrict (fromIntegral int :: Word8)
           | isIntX 16 int = cons 201 $ encodeStrict (fromIntegral int :: Word16)
           | isIntX 32 int = cons 202 $ encodeStrict (fromIntegral int :: Word32)
           | isIntX 64 int = cons 203 $ encodeStrict (fromIntegral int :: Word64)

instance PackStream Integer where
  pack int | isIntX 64 int = pack (fromIntegral int :: Int)
           | otherwise     = error "Cannot pack so large Integer"

instance PackStream Double where
  pack dbl = cons 193 $ encodeStrict (doubleToWord dbl)

instance PackStream Float where
  pack flt = cons 193 $ encodeStrict (doubleToWord (float2Double flt))

instance PackStream Text where
  pack text = mkPackedCollection (B.length pbs) pbs (128, 208, 209, 210)
    where pbs = encodeUtf8 text

instance PackStream a => PackStream [a] where
  pack lst = mkPackedCollection (length lst) pbs (144, 212, 213, 214)
    where pbs = B.concat $ map pack lst

instance PackStream a => PackStream (Map Text a) where
  pack dict = mkPackedCollection (size dict) pbs (160, 216, 217, 218)
    where pbs = B.concat $ map pack $ concatMap mkPair $ assocs dict
          mkPair (key, val) = [PS key, PS val]

instance PackStream PS where
  pack (PS a) = pack a

inRange :: Ord a => (a, a) -> a -> Bool
inRange (low, up) x = low <= x && x < up

isTinyInt :: Integral a => a -> Bool
isTinyInt = inRange (-2^4, 2^7 - 1)

isIntX :: Integral x => x -> x -> Bool
isIntX p = inRange (-2^(p-1), 2^(p-1) - 1)

encodeStrict :: Binary a => a -> ByteString
encodeStrict = toStrict . encode

mkPackedCollection :: Int -> ByteString -> (Word8, Word8, Word8, Word8) -> ByteString
mkPackedCollection len bst (wt, w8, w16, w32) = helper len
  where helper len | len < 2^4  = cons (wt + fromIntegral len) bst
                   | len < 2^8  = cons w8 $ cons (fromIntegral len) bst
                   | len < 2^16 = cons w16 $ encodeStrict (fromIntegral len :: Word16) `append` bst
                   | len < 2^32 = cons w32 $ encodeStrict (fromIntegral len :: Word32) `append` bst
                   | otherwise  = error "Cannot pack so large collection"
