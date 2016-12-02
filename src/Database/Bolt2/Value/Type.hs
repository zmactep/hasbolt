module Database.Bolt2.Value.Type where

import           Control.Monad.Trans.State (StateT (..), evalStateT)
import           Data.ByteString           (ByteString)
import           Data.Map.Strict           (Map (..))
import           Data.Text                 (Text)
import           Data.Word                 (Word8)

-- \The 'UnpackT' transformer helps to unpack a set of values from one 'ByteString'
type UnpackT = StateT ByteString

-- |The 'Structure' datatype describes Neo4j structure for BOLT protocol
data Structure = Structure { signature :: Word8
                           , fields    :: [Value]
                           }
  deriving (Show, Eq)

-- |The 'BoltValue' class describes values, that can be packed and unpacked for BOLT protocol.
class BoltValue a where
  -- |Packs a value to 'ByteString'
  pack :: a -> ByteString
  -- |Unpacks in a State monad to get values from single 'ByteString'
  unpackT :: Monad m => UnpackT m a

  -- |Unpacks a 'ByteString' to selected value
  unpack :: Monad m  => ByteString -> m a
  unpack = evalStateT unpackT

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
