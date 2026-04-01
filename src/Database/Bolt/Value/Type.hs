{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies #-}
module Database.Bolt.Value.Type where

import           Control.DeepSeq      (NFData)
import           Control.Monad.Except (ExceptT, MonadError (..))
import           Control.Monad.Fail   as Fail (MonadFail (..))
import           Control.Monad.State  (MonadState (..), StateT (..))
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty   (NonEmpty (..), toList)
import           Data.Map.Strict      (Map, fromList)
import           Data.Text            (Text)
import qualified Data.Text            as T (pack, unpack)
import           Data.Word            (Word8)
import           GHC.Generics         (Generic)
import           GHC.Stack            (HasCallStack)

-- |Error during unpack process
data UnpackError = NotNull
                 | NotInt
                 | NotFloat
                 | NotString
                 | NotBool
                 | NotList
                 | NotDict
                 | NotStructure
                 | NotValue
                 | BinaryError Text
                 | Not Text
  deriving (Eq, Ord)

instance Show UnpackError where
  show NotNull      = "Not a Null value"
  show NotInt       = "Not an Int value"
  show NotFloat     = "Not a Float value"
  show NotString    = "Not a String value"
  show NotBool      = "Not a Bool value"
  show NotList      = "Not a List value"
  show NotDict      = "Not a Dict value"
  show NotStructure = "Not a Structure value"
  show NotValue     = "Not a Value value"
  show (BinaryError what) = "Error while decoding binary format: " <> T.unpack what
  show (Not what)   = "Not a " <> T.unpack what <> " (Structure) value"

-- |The 'UnpackT' transformer helps to unpack a set of values from one 'ByteString'
newtype UnpackT m a = UnpackT { runUnpackT :: ExceptT UnpackError (StateT ByteString m) a }
  deriving newtype (Functor, Applicative, Monad, MonadError UnpackError, MonadState ByteString)

-- |The 'Structure' datatype describes Neo4j structure for BOLT protocol
data Structure = Structure { signature :: Word8
                           , fields    :: [Value]
                           }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- |Generalizes all datatypes that can be deserialized from 'Structure's.
class FromStructure a where
  fromStructure :: (HasCallStack, MonadError UnpackError m) => Structure -> m a

-- |Generalizes all datatypes that can be serialized to 'Structure's.
class ToStructure a where
  toStructure :: a -> Structure

-- |The 'BoltValue' class describes values, that can be packed and unpacked for BOLT protocol.
class BoltValue a where
  -- |Packs a value to 'ByteString'
  pack :: HasCallStack => a -> Put
  -- |Unpacks in a State monad to get values from single 'ByteString'
  unpackT :: HasCallStack => Get a

-- |Unpacks a 'ByteString' to selected value
unpack :: (Monad m, BoltValue a, HasCallStack)  => ByteString -> m (Either UnpackError a)
unpack = pure . unpackAction unpackT . fromStrict

-- |Old-style unpack that runs 'fail' on error
unpackF :: (MonadFail m, BoltValue a) => ByteString -> m a
unpackF bs = do let result = unpackAction unpackT $ fromStrict bs
                case result of
                  Right x -> pure x
                  Left  e -> Fail.fail $ show e

-- |Unpacks a 'ByteString' to selected value by some custom action
unpackAction :: HasCallStack => Get a -> BSL.ByteString -> Either UnpackError a
unpackAction action bs = case runGetOrFail action bs of
  Left (_, _, err) -> Left $ BinaryError $ T.pack err
  Right (_, _, a) -> Right a

-- |The 'Value' datatype generalizes all primitive 'BoltValue's
data Value = N ()
           | B Bool
           | I Int
           | F Double
           | T Text
           | L [Value]
           | M (Map Text Value)
           | S Structure
           | Bytes ByteString -- ^Raw byte data. PackStream Bytes type.
                              -- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- |Every datatype that can be represented as BOLT protocol value
class IsValue a where
  -- |Wraps value with 'Value' constructor
  toValue :: HasCallStack => a -> Value
  -- |How to represent a list of values
  toValueList :: HasCallStack => [a] -> Value
  toValueList = L . fmap toValue

instance IsValue () where
  toValue = N

instance IsValue Bool where
  toValue = B

instance IsValue Int where
  toValue = I

instance IsValue Integer where
  toValue = I . fromIntegral

instance IsValue Double where
  toValue = F

instance IsValue Float where
  toValue = F . realToFrac

instance IsValue Text where
  toValue = T

instance IsValue Char where
  toValue = toValueList . pure
  toValueList = T . T.pack

instance IsValue a => IsValue [a] where
  toValue = toValueList

instance IsValue a => IsValue (NonEmpty a) where
  toValue = toValue . toList

instance IsValue a => IsValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue _        = N ()

-- |Allows 'ByteString' to be used as a BOLT value via the 'Bytes' constructor.
-- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
instance IsValue ByteString where
  toValue = Bytes

instance IsValue (Map Text Value) where
  toValue = M

-- | IsValue instances for tuples (2-15 elements)
-- Tuples are converted to BOLT lists.

instance (IsValue a, IsValue b) => IsValue (a, b) where
  toValue (a, b) = L [toValue a, toValue b]

instance (IsValue a, IsValue b, IsValue c) => IsValue (a, b, c) where
  toValue (a, b, c) = L [toValue a, toValue b, toValue c]

instance (IsValue a, IsValue b, IsValue c, IsValue d) => IsValue (a, b, c, d) where
  toValue (a, b, c, d) = L [toValue a, toValue b, toValue c, toValue d]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e) => IsValue (a, b, c, d, e) where
  toValue (a, b, c, d, e) = L [toValue a, toValue b, toValue c, toValue d, toValue e]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f) => IsValue (a, b, c, d, e, f) where
  toValue (a, b, c, d, e, f) = L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g) => IsValue (a, b, c, d, e, f, g) where
  toValue (a, b, c, d, e, f, g) = L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h) => IsValue (a, b, c, d, e, f, g, h) where
  toValue (a, b, c, d, e, f, g, h) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i) => IsValue (a, b, c, d, e, f, g, h, i) where
  toValue (a, b, c, d, e, f, g, h, i) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j) => IsValue (a, b, c, d, e, f, g, h, i, j) where
  toValue (a, b, c, d, e, f, g, h, i, j) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k) => IsValue (a, b, c, d, e, f, g, h, i, j, k) where
  toValue (a, b, c, d, e, f, g, h, i, j, k) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j, toValue k]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l) => IsValue (a, b, c, d, e, f, g, h, i, j, k, l) where
  toValue (a, b, c, d, e, f, g, h, i, j, k, l) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j, toValue k, toValue l]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m) => IsValue (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  toValue (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j, toValue k, toValue l, toValue m]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n) => IsValue (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  toValue (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j, toValue k, toValue l, toValue m, toValue n]

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o) => IsValue (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  toValue (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    L [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f, toValue g, toValue h, toValue i, toValue j, toValue k, toValue l, toValue m, toValue n, toValue o]

-- |Wrap key-value pair with 'Value' datatype
(=:) :: IsValue a => Text -> a -> (Text, Value)
(=:) key val = (key, toValue val)

-- |Construct properties map from list
props :: [(Text, Value)] -> Map Text Value
props = fromList

-- = Structure types

-- == Neo4j subjects

data Node = Node { nodeIdentity  :: Int             -- ^Neo4j node identifier
                 , labels        :: [Text]          -- ^Set of node labels (types)
                 , nodeProps     :: Map Text Value  -- ^Dict of node properties
                 , nodeElementId :: Text            -- ^Element ID string (v5+, empty for v3)
                 }
  deriving (Show, Eq)

data Relationship = Relationship { relIdentity        :: Int            -- ^Neo4j relationship identifier
                                 , startNodeId        :: Int            -- ^Identifier of start node
                                 , endNodeId          :: Int            -- ^Identifier of end node
                                 , relType            :: Text           -- ^Relationship type
                                 , relProps           :: Map Text Value -- ^Dict of relationship properties
                                 , relElementId       :: Text           -- ^Element ID string (v5+, empty for v3)
                                 , startNodeElementId :: Text           -- ^Start node element ID (v5+, empty for v3)
                                 , endNodeElementId   :: Text           -- ^End node element ID (v5+, empty for v3)
                                 }
  deriving (Show, Eq)

data URelationship = URelationship { urelIdentity  :: Int            -- ^Neo4j relationship identifier
                                   , urelType      :: Text           -- ^Relationship type
                                   , urelProps     :: Map Text Value -- ^Dict of relationship properties
                                   , urelElementId :: Text           -- ^Element ID string (v5+, empty for v3)
                                   }
  deriving (Show, Eq)

data Path = Path { pathNodes         :: [Node]          -- ^Chain of 'Node's in path
                 , pathRelationships :: [URelationship] -- ^Chain of 'Relationship's in path
                 , pathSequence      :: [Int]           -- ^Path sequence
                 }
  deriving (Show, Eq)
