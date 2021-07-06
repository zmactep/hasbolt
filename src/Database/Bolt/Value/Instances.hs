{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Value.Instances where

import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type

import           Control.Applicative          (pure)
import           Control.Monad                (forM, replicateM)
import           Control.Monad.Except         (MonadError (..))
import           Data.Binary                  (Binary (..), decode, encode)
import           Data.Binary.Get
import           Data.Binary.IEEE754          (doubleToWord, wordToDouble)
import           Data.ByteString              (ByteString, append, cons, singleton)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Int
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Word

instance BoltValue () where
  pack () = singleton nullCode

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | m == nullCode = pure ()
                           | otherwise     = fail "expected null"

instance BoltValue Bool where
  pack True  = singleton trueCode
  pack False = singleton falseCode

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | m == trueCode  = pure True
                           | m == falseCode = pure False
                           | otherwise      = fail "expected bool"

instance BoltValue Int where
  pack int | isTinyInt int = encodeStrict (fromIntegral int :: Word8)
           | isIntX  8 int = cons  int8Code $ encodeStrict (fromIntegral int :: Word8)
           | isIntX 16 int = cons int16Code $ encodeStrict (fromIntegral int :: Word16)
           | isIntX 32 int = cons int32Code $ encodeStrict (fromIntegral int :: Word32)
           | isIntX 62 int = cons int64Code $ encodeStrict (fromIntegral int :: Word64)
           | otherwise     = error "Cannot pack so large integer"

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | isTinyWord m   = pure . toInt $ (fromIntegral m :: Int8)
                           | m == int8Code  = toInt <$> getInt8
                           | m == int16Code = toInt <$> getInt16be
                           | m == int32Code = toInt <$> getInt32be
                           | m == int64Code = toInt <$> getInt64be
                           | otherwise      = fail "expected int"

instance BoltValue Double where
  pack dbl = cons doubleCode $ encodeStrict (doubleToWord dbl)

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | m == doubleCode = wordToDouble <$> getWord64be
                           | otherwise       = fail "expected double"

instance BoltValue Text where
  pack txt = mkPackedCollection (B.length pbs) pbs (textConst, text8Code, text16Code, text32Code)
    where pbs = encodeUtf8 txt

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | isTinyText m    = unpackTextBySize (getSize m)
                           | m == text8Code  = toInt <$> getInt8 >>= unpackTextBySize
                           | m == text16Code = toInt <$> getInt16be >>= unpackTextBySize
                           | m == text32Code = toInt <$> getInt32be >>= unpackTextBySize
                           | otherwise       = fail "expected text"
          unpackTextBySize size = do str <- getByteString size
                                     pure $! decodeUtf8 str

instance BoltValue a => BoltValue [a] where
  pack lst = mkPackedCollection (length lst) pbs (listConst, list8Code, list16Code, list32Code)
    where pbs = B.concat $ map pack lst

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | isTinyList m    = unpackListBySize (getSize m)
                           | m == list8Code  = toInt <$> getInt8 >>= unpackListBySize
                           | m == list16Code = toInt <$> getInt16be >>= unpackListBySize
                           | m == list32Code = toInt <$> getInt32be >>= unpackListBySize
                           | otherwise       = fail "expected list"
          unpackListBySize size = forM [1..size] $ const unpackT

instance BoltValue a => BoltValue (Map Text a) where
  pack dict = mkPackedCollection (M.size dict) pbs (dictConst, dict8Code, dict16Code, dict32Code)
    where pbs = B.concat $ map mkPairPack $ M.assocs dict
          mkPairPack (key, val) = pack key `append` pack val

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | isTinyDict m    = unpackDictBySize (getSize m)
                           | m == dict8Code  = toInt <$> getInt16be >>= unpackDictBySize
                           | m == dict16Code = toInt <$> getInt16be >>= unpackDictBySize
                           | m == dict32Code = toInt <$> getInt32be >>= unpackDictBySize
                           | otherwise       = fail "expected dict"
          unpackDictBySize = (M.fromList <$>) . unpackPairsBySize
          unpackPairsBySize size = forM [1..size] $ const $ do
                                     !key <- unpackT
                                     !value <- unpackT
                                     pure (key, value)

instance BoltValue Structure where
  pack (Structure sig lst) | size < size4  = (structConst + fromIntegral size) `cons` pData
                           | size < size8  = struct8Code `cons` fromIntegral size `cons` pData
                           | size < size16 = struct16Code `cons` encodeStrict size `append` pData
                           | otherwise     = error "Cannot pack so large structure"
    where size = fromIntegral $ length lst :: Word16
          pData = sig `cons` B.concat (map pack lst)

  unpackT = getWord8 >>= unpackByMarker
    where unpackByMarker m | isTinyStruct m    = unpackStructureBySize (getSize m)
                           | m == struct8Code  = toInt <$> getInt8 >>= unpackStructureBySize
                           | m == struct16Code = toInt <$> getInt16be >>= unpackStructureBySize
                           | otherwise         = fail "expected structure"
          unpackStructureBySize size = Structure <$> getWord8 <*> replicateM size unpackT

instance BoltValue Value where
  pack (N n) = pack n
  pack (B b) = pack b
  pack (I i) = pack i
  pack (F d) = pack d
  pack (T t) = pack t
  pack (L l) = pack l
  pack (M m) = pack m
  pack (S s) = pack s

  unpackT = lookAhead getWord8 >>= unpackByMarker
    where unpackByMarker m | isNull   m = N <$> unpackT
                           | isBool   m = B <$> unpackT
                           | isInt    m = I <$> unpackT
                           | isDouble m = F <$> unpackT
                           | isText   m = T <$> unpackT
                           | isList   m = L <$> unpackT
                           | isDict   m = M <$> unpackT
                           | isStruct m = S <$> unpackT
                           | otherwise  = fail "not value"

-- = Structure instances for Neo4j structures

instance FromStructure Node where
  fromStructure struct =
    case struct of
      (Structure sig [I nid, L vlbls, M prps]) | sig == sigNode -> flip (Node nid) prps <$> cnvT vlbls
      _                                                         -> throwError $ Not "Node"
    where
      cnvT []       = pure []
      cnvT (T x:xs) = (x:) <$> cnvT xs
      cnvT _        = throwError NotString

instance FromStructure Relationship where
  fromStructure struct =
    case struct of
      (Structure sig [I rid, I sni, I eni, T rt, M rp]) | sig == sigRel -> pure $ Relationship rid sni eni rt rp
      _                                                                 -> throwError $ Not "Relationship"

instance FromStructure URelationship where
  fromStructure struct =
    case struct of
      (Structure sig [I rid, T rt, M rp]) | sig == sigURel -> pure $ URelationship rid rt rp
      _                                                    -> throwError $ Not "URelationship"

instance FromStructure Path where
  fromStructure struct =
    case struct of
      (Structure sig [L vnp, L vrp, L vip]) | sig == sigPath -> Path <$> cnvN vnp <*> cnvR vrp <*> cnvI vip
      _                                                      -> throwError $ Not "Path"
    where
      cnvN []       = pure []
      cnvN (S x:xs) = (:) <$> fromStructure x <*> cnvN xs
      cnvN _        = throwError $ Not "Node"

      cnvR []       = pure []
      cnvR (S x:xs) = (:) <$> fromStructure x <*> cnvR xs
      cnvR _        = throwError NotStructure

      cnvI []       = pure []
      cnvI (I x:xs) = (x:) <$> cnvI xs
      cnvI _        = throwError NotInt


decodeStrict :: Binary a => ByteString -> a
decodeStrict = decode . fromStrict

encodeStrict :: Binary a => a -> ByteString
encodeStrict = toStrict . encode

-- |Pack collection using it's size and set of BOLT constants
mkPackedCollection :: Int -> ByteString -> (Word8, Word8, Word8, Word8) -> ByteString
mkPackedCollection size bst (wt, w8, w16, w32)
  | size < size4  = cons (wt + fromIntegral size) bst
  | size < size8  = cons w8 $ cons (fromIntegral size) bst
  | size < size16 = cons w16 $ encodeStrict (fromIntegral size :: Word16) `append` bst
  | size < size32 = cons w32 $ encodeStrict (fromIntegral size :: Word32) `append` bst
  | otherwise  = error "Cannot pack so large collection"

size4,size8, size16,size32 :: Integral a => a
size4   = 2^(4  :: Int)
size8   = 2^(8  :: Int)
size16  = 2^(16 :: Int)
size32  = 2^(32 :: Int)
