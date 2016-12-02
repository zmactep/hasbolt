module Database.Bolt.Internal.Unpack.Structure
    ( UnpackStream (..), UnpackT (..)
    , NeoValue (..)
    , Node (..), Relationship (..), UnboundRelationship (..), Path (..)
    , runUnpackT
    , unpackSizeAndSig
    ) where

import           Control.Applicative                        ((<$>))
import           Data.Binary                                (decode)
import           Data.ByteString.Lazy                       (fromStrict)
import           Data.Map.Strict                            (Map (..))
import           Data.Text                                  (Text)
import           Data.Word                                  (Word8)

import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Dict
import           Database.Bolt.Internal.Unpack.List
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.Primitive
import           Database.Bolt.Internal.Unpack.Text
import           Database.Bolt.Internal.Unpack.UnpackStream

data NeoValue = N ()
              | B Bool
              | I Int
              | F Double
              | T Text
              | L [NeoValue]
              | M (Map Text NeoValue)
              | Nd Node
              | Rl Relationship
              | Ur UnboundRelationship
              | Pt Path
  deriving (Show, Eq)

data Node = Node { nodeIdentity :: Int
                 , labels       :: [Text]
                 , nodeProps    :: Map Text NeoValue
                 }
  deriving (Show, Eq)

data Relationship = Relationship { relIdentity :: Int
                                 , startNodeId :: Int
                                 , endNodeId   :: Int
                                 , relType     :: Text
                                 , relProps    :: Map Text NeoValue
                                 }
  deriving (Show, Eq)

data UnboundRelationship = UnboundRelationship { urelIdentity :: Int
                                               , urelType     :: Text
                                               , urelProps    :: Map Text NeoValue
                                               }
  deriving (Show, Eq)

data Path = Path { pathNodes         :: [Node]
                 , pathRelationships :: [UnboundRelationship]
                 , pathSequence      :: [Int]
                 }
  deriving (Show, Eq)

instance UnpackStream Node where
  unpack = do (_, sig) <- unpackSizeAndSig
              unpackNodeBySignature sig

instance UnpackStream Relationship where
  unpack = do (_, sig) <- unpackSizeAndSig
              unpackRelationshipBySignature sig

instance UnpackStream UnboundRelationship where
  unpack = do (_, sig) <- unpackSizeAndSig
              unpackURelationshipBySignature sig

instance UnpackStream Path where
  unpack = do (_, sig) <- unpackSizeAndSig
              unpackPathBySignature sig

instance UnpackStream NeoValue where
  unpack = firstByte >>= unpackByFirstByte

unpackByFirstByte :: Monad m => Word8 -> UnpackT m NeoValue
unpackByFirstByte w | isNull   w = N <$> unpack
                    | isBool   w = B <$> unpack
                    | isInt    w = I <$> unpack
                    | isDouble w = F <$> unpack
                    | isText   w = T <$> unpack
                    | isList   w = L <$> unpack
                    | isDict   w = M <$> unpack
                    | isStruct w = unpackStruct
                    | otherwise  = fail "Not recognisable type"

unpackStruct :: Monad m => UnpackT m NeoValue
unpackStruct = do (_, sig) <- unpackSizeAndSig
                  unpackStructBySig sig

firstByte :: Monad m => UnpackT m Word8
firstByte = do w <- topBS 1
               return $ decode (fromStrict w)

unpackSizeAndSig :: Monad m => UnpackT m (Int, Word8)
unpackSizeAndSig = do marker <- unpackW8
                      size <- getStructureSize marker
                      sig <- unpackW8
                      return (size, sig)

unpackStructBySig :: Monad m => Word8 -> UnpackT m NeoValue
unpackStructBySig w | isNode w          = Nd <$> unpackNodeBySignature w
                    | isRelationship w  = Rl <$> unpackRelationshipBySignature w
                    | isURelationship w = Ur <$> unpackURelationshipBySignature w
                    | isPath w          = Pt <$> unpackPathBySignature w

unpackNodeBySignature :: Monad m => Word8 -> UnpackT m Node
unpackNodeBySignature w | isNode w  = do nid <- unpack
                                         nlbls <- unpack
                                         nprps <- unpack
                                         return $ Node nid nlbls nprps
                        | otherwise = fail "Not a Node value"

unpackRelationshipBySignature :: Monad m => Word8 -> UnpackT m Relationship
unpackRelationshipBySignature w | isRelationship w = do rid <- unpack
                                                        rsn <- unpack
                                                        ren <- unpack
                                                        rtpe <- unpack
                                                        rprps <- unpack
                                                        return $ Relationship rid rsn ren rtpe rprps
                                | otherwise = fail "Not a Relationship value"

unpackURelationshipBySignature :: Monad m => Word8 -> UnpackT m UnboundRelationship
unpackURelationshipBySignature w | isURelationship w = do rid <- unpack
                                                          rtpe <- unpack
                                                          rprps <- unpack
                                                          return $ UnboundRelationship rid rtpe rprps
                                 | otherwise = fail "Not an Unbound Relationship value"


unpackPathBySignature :: Monad m => Word8 -> UnpackT m Path
unpackPathBySignature w | isRelationship w = do pn <- unpack
                                                pr <- unpack
                                                ps <- unpack
                                                return $ Path pn pr ps
                        | otherwise = fail "Not a Path value"

getStructureSize :: Monad m => Word8 -> UnpackT m Int
getStructureSize m | isTinyStruct m    = return $ getSize m
                   | m == struct8Code  = convertToInt <$> unpackW8
                   | m == struct16Code = convertToInt <$> unpackW16
                   | otherwise         = fail "Not a structure value"
