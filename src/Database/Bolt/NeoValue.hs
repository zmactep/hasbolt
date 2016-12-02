module Database.Bolt.NeoValue
    ( PackStream (..), UnpackStream (..)
    , UnpackT (..), runUnpackT
    , NeoValue (..)
    , Node (..), Relationship (..), UnboundRelationship (..), Path (..)
    ) where

import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.PackStream
import           Database.Bolt.Internal.Unpack.Dict
import           Database.Bolt.Internal.Unpack.List
import           Database.Bolt.Internal.Unpack.Number
import           Database.Bolt.Internal.Unpack.Primitive
import           Database.Bolt.Internal.Unpack.Structure
import           Database.Bolt.Internal.Unpack.Text
import           Database.Bolt.Internal.Unpack.UnpackStream

instance PackStream NeoValue where
  pack (N n)  = pack n
  pack (B v)  = pack v
  pack (I i)  = pack i
  pack (F f)  = pack f
  pack (T t)  = pack t
  pack (L l)  = pack l
  pack (M m)  = pack m
  pack _      = error "Structure type packing is not implemented"
