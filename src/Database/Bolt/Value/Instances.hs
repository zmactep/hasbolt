{-# LANGUAGE FlexibleInstances #-}

module Database.Bolt.Value.Instances where

import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type

import           Control.Monad                (forM, replicateM)
import           Control.Monad.Trans.State    (gets, modify)
import           Data.Binary                  (Binary (..), decode, encode)
import           Data.Binary.IEEE754          (doubleToWord, wordToDouble)
import           Data.ByteString              (ByteString, append, cons, drop,
                                               singleton, take)
import qualified Data.ByteString              as B (concat, length)
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Int
import           Data.Map.Strict              (Map (..), assocs, size, fromList)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Word
import           Prelude                      hiding (drop, take)

instance BoltValue () where
  pack () = singleton nullCode

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | m == nullCode = return ()
                           | otherwise     = fail "Not a Null value"

instance BoltValue Bool where
  pack True  = singleton trueCode
  pack False = singleton falseCode

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | m == trueCode  = return True
                           | m == falseCode = return False
                           | otherwise      = fail "Not a Bool value"

instance BoltValue Int where
  pack int | isTinyInt int = encodeStrict (fromIntegral int :: Word8)
           | isIntX  8 int = cons  int8Code $ encodeStrict (fromIntegral int :: Word8)
           | isIntX 16 int = cons int16Code $ encodeStrict (fromIntegral int :: Word16)
           | isIntX 32 int = cons int32Code $ encodeStrict (fromIntegral int :: Word32)
           | isIntX 64 int = cons int64Code $ encodeStrict (fromIntegral int :: Word64)

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | isTinyWord m   = return . toInt $ (fromIntegral m :: Int8)
                           | m == int8Code  = toInt <$> unpackI8
                           | m == int16Code = toInt <$> unpackI16
                           | m == int32Code = toInt <$> unpackI32
                           | m == int64Code = toInt <$> unpackI64
                           | otherwise      = fail "Not an Int value"

instance BoltValue Double where
  pack dbl = cons doubleCode $ encodeStrict (doubleToWord dbl)

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | m == doubleCode = wordToDouble <$> unpackW64
                           | otherwise       = fail "Not a Double value"

instance BoltValue Text where
  pack txt = mkPackedCollection (B.length pbs) pbs (textConst, text8Code, text16Code, text32Code)
    where pbs = encodeUtf8 txt

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | isTinyText m    = unpackTextBySize (getSize m)
                           | m == text8Code  = toInt <$> unpackW8 >>= unpackTextBySize
                           | m == text16Code = toInt <$> unpackW16 >>= unpackTextBySize
                           | m == text32Code = toInt <$> unpackW32 >>= unpackTextBySize
                           | otherwise       = fail "Not a Text value"
          unpackTextBySize size = do str <- gets (take size)
                                     modify (drop size)
                                     return $ decodeUtf8 str

instance BoltValue a => BoltValue [a] where
  pack lst = mkPackedCollection (length lst) pbs (listConst, list8Code, list16Code, list32Code)
    where pbs = B.concat $ map pack lst

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | isTinyList m    = unpackListBySize (getSize m)
                           | m == list8Code  = toInt <$> unpackW8 >>= unpackListBySize
                           | m == list16Code = toInt <$> unpackW16 >>= unpackListBySize
                           | m == list32Code = toInt <$> unpackW32 >>= unpackListBySize
                           | otherwise       = fail "Not a List value"
          unpackListBySize size = forM [1..size] $ const unpackT

instance BoltValue a => BoltValue (Map Text a) where
  pack dict = mkPackedCollection (size dict) pbs (dictConst, dict8Code, dict16Code, dict32Code)
    where pbs = B.concat $ map mkPairPack $ assocs dict
          mkPairPack (key, val) = pack key `append` pack val

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | isTinyDict m    = unpackDictBySize (getSize m)
                           | m == dict8Code  = toInt <$> unpackW8 >>= unpackDictBySize
                           | m == dict16Code = toInt <$> unpackW16 >>= unpackDictBySize
                           | m == dict32Code = toInt <$> unpackW32 >>= unpackDictBySize
                           | otherwise       = error "Not a Dict value"
          unpackDictBySize = (fromList <$>) . unpackPairsBySize
          unpackPairsBySize size = forM [1..size] $ const $ do
                                     key <- unpackT
                                     value <- unpackT
                                     return (key, value)

instance BoltValue Structure where
  pack (Structure sig fields) | size < 16   = (structConst + fromIntegral size) `cons` pData
                              | size < 2^8  = struct8Code `cons` fromIntegral size `cons` pData
                              | size < 2^16 = struct16Code `cons` encodeStrict size `append` pData
    where size = fromIntegral $ length fields :: Word16
          pData = sig `cons` B.concat (map pack fields)

  unpackT = unpackW8 >>= unpackByMarker
    where unpackByMarker m | isTinyStruct m    = unpackStructureBySize (getSize m)
                           | m == struct8Code  = toInt <$> unpackW8 >>= unpackStructureBySize
                           | m == struct16Code = toInt <$> unpackW16 >>= unpackStructureBySize
                           | otherwise         = fail "Not a Structure value"
          unpackStructureBySize size = do sig <- unpackW8
                                          fields <- replicateM size unpackT
                                          return $ Structure sig fields

instance BoltValue Value where
  pack (N n) = pack n
  pack (B b) = pack b
  pack (I i) = pack i
  pack (F d) = pack d
  pack (T t) = pack t
  pack (L l) = pack l
  pack (M m) = pack m
  pack (S s) = pack s

  unpackT = observeW8 >>= unpackByMarker
    where unpackByMarker m | isNull   m = N <$> unpackT
                           | isBool   m = B <$> unpackT
                           | isInt    m = I <$> unpackT
                           | isDouble m = F <$> unpackT
                           | isText   m = T <$> unpackT
                           | isList   m = L <$> unpackT
                           | isDict   m = M <$> unpackT
                           | isStruct m = S <$> unpackT
                           | otherwise  = fail "Not a Value value"

-- |Structure unpack function
unpackS :: (Monad m, Structable a) => ByteString -> m a
unpackS bs = unpack bs >>= fromStructure

-- = Integer values unpackers

observeW8 :: Monad m => UnpackT m Word8
observeW8 = observeNum 1

unpackW8 :: Monad m => UnpackT m Word8
unpackW8 = unpackNum 1

unpackW16 :: Monad m => UnpackT m Word16
unpackW16 = unpackNum 2

unpackW32 :: Monad m => UnpackT m Word32
unpackW32 = unpackNum 4

unpackW64 :: Monad m => UnpackT m Word64
unpackW64 = unpackNum 8

unpackI8 :: Monad m => UnpackT m Int8
unpackI8 = unpackNum 1

unpackI16 :: Monad m => UnpackT m Int16
unpackI16 = unpackNum 2

unpackI32 :: Monad m => UnpackT m Int32
unpackI32 = unpackNum 4

unpackI64 :: Monad m => UnpackT m Int64
unpackI64 = unpackNum 8

-- = Other helpers

-- |Unpacks n bytes as a numeric type
observeNum :: (Monad m, Binary a, Num a) => Int -> UnpackT m a
observeNum = (decodeStrict <$>) . topBS

unpackNum :: (Monad m, Binary a, Num a) => Int -> UnpackT m a
unpackNum = (decodeStrict <$>) . popBS

decodeStrict :: Binary a => ByteString -> a
decodeStrict = decode . fromStrict

encodeStrict :: Binary a => a -> ByteString
encodeStrict = toStrict . encode

-- |Obtain first n bytes of 'ByteString'
topBS :: Monad m => Int -> UnpackT m ByteString
topBS size = gets (take size)

-- |Obtain first n bytes of 'ByteString' and move offset by n
popBS :: Monad m => Int -> UnpackT m ByteString
popBS size = do top <- topBS size
                modify (drop size)
                return top

-- |Pack collection using it's size and set of BOLT constants
mkPackedCollection :: Int -> ByteString -> (Word8, Word8, Word8, Word8) -> ByteString
mkPackedCollection size bst (wt, w8, w16, w32) = helper size
  where helper size | size < 2^4  = cons (wt + fromIntegral size) bst
                    | size < 2^8  = cons w8 $ cons (fromIntegral size) bst
                    | size < 2^16 = cons w16 $ encodeStrict (fromIntegral size :: Word16) `append` bst
                    | size < 2^32 = cons w32 $ encodeStrict (fromIntegral size :: Word32) `append` bst
                    | otherwise  = error "Cannot pack so large collection"
