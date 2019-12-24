{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Bolt.Value.Type where

import           Control.Monad.Fail        as Fail (MonadFail (..))
import           Control.Monad.State       (MonadState (..), StateT (..), evalStateT)
import           Control.Monad.Except      (MonadError (..), ExceptT, runExceptT)
import           Data.ByteString           (ByteString)
import           Data.Map.Strict           (Map, fromList)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T (unpack, pack)
import           Data.Word                 (Word8)

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
  show (Not what)   = "Not a " <> T.unpack what <> " (Structure) value"

-- |The 'UnpackT' transformer helps to unpack a set of values from one 'ByteString'
newtype UnpackT m a = UnpackT { runUnpackT :: ExceptT UnpackError (StateT ByteString m) a }
  deriving (Functor, Applicative, Monad, MonadError UnpackError, MonadState ByteString)

-- |The 'Structure' datatype describes Neo4j structure for BOLT protocol
data Structure = Structure { signature :: Word8
                           , fields    :: [Value]
                           }
  deriving (Show, Eq)

-- |Generalizes all datatypes that can be deserialized from 'Structure's.
class FromStructure a where
  fromStructure :: MonadError UnpackError m => Structure -> m a

-- |Generalizes all datatypes that can be serialized to 'Structure's.
class ToStructure a where
  toStructure :: a -> Structure

-- |The 'BoltValue' class describes values, that can be packed and unpacked for BOLT protocol.
class BoltValue a where
  -- |Packs a value to 'ByteString'
  pack :: a -> ByteString
  -- |Unpacks in a State monad to get values from single 'ByteString'
  unpackT :: Monad m => UnpackT m a

-- |Unpacks a 'ByteString' to selected value
unpack :: (Monad m, BoltValue a)  => ByteString -> m (Either UnpackError a)
unpack = unpackAction unpackT

-- |Old-style unpack that runs 'fail' on error
unpackF :: (MonadFail m, BoltValue a) => ByteString -> m a
unpackF bs = do result <- unpack bs
                case result of
                  Right x -> pure x
                  Left  e -> Fail.fail $ show e

-- |Unpacks a 'ByteString' to selected value by some custom action
unpackAction :: Monad m => UnpackT m a -> ByteString -> m (Either UnpackError a)
unpackAction action = evalStateT (runExceptT $ runUnpackT action) 

-- |The 'Value' datatype generalizes all primitive 'BoltValue's
data Value = N ()
           | B Bool
           | I Int
           | F Double
           | T Text
           | L [Value]
           | M (Map Text Value)
           | S Structure
  deriving (Show, Eq)

-- |Every datatype that can be represented as BOLT protocol value
class IsValue a where
  -- |Wraps value with 'Value' constructor
  toValue :: a -> Value
  -- |How to represent a list of values
  toValueList :: [a] -> Value
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

instance IsValue (Map Text Value) where
  toValue = M

-- |Wrap key-value pair with 'Value' datatype
(=:) :: IsValue a => Text -> a -> (Text, Value)
(=:) key val = (key, toValue val)

-- |Construct properties map from list
props :: [(Text, Value)] -> Map Text Value
props = fromList

-- = Structure types

-- == Neo4j subjects

data Node = Node { nodeIdentity :: Int             -- ^Neo4j node identifier
                 , labels       :: [Text]          -- ^Set of node labels (types)
                 , nodeProps    :: Map Text Value  -- ^Dict of node properties
                 }
  deriving (Show, Eq)

data Relationship = Relationship { relIdentity :: Int            -- ^Neo4j relationship identifier
                                 , startNodeId :: Int            -- ^Identifier of start node
                                 , endNodeId   :: Int            -- ^Identifier of end node
                                 , relType     :: Text           -- ^Relationship type
                                 , relProps    :: Map Text Value -- ^Dict of relationship properties
                                 }
  deriving (Show, Eq)

data URelationship = URelationship { urelIdentity :: Int            -- ^Neo4j relationship identifier
                                   , urelType     :: Text           -- ^Relationship type
                                   , urelProps    :: Map Text Value -- ^Dict of relationship properties
                                   }
  deriving (Show, Eq)

data Path = Path { pathNodes         :: [Node]          -- ^Chain of 'Node's in path
                 , pathRelationships :: [URelationship] -- ^Chain of 'Relationship's in path
                 , pathSequence      :: [Int]           -- ^Path sequence
                 }
  deriving (Show, Eq)

