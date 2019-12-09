{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Record where

import           Database.Bolt.Value.Type
import           Database.Bolt.Value.Instances      ()

import           Control.Monad.Except               (MonadError (..))
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as M
import           Data.Text                          (Text)

-- |Result type for query requests
type Record = Map Text Value

-- |Get exact type from Value
class RecordValue a where
  exactEither :: Value -> Either UnpackError a

exact :: (MonadError UnpackError m, RecordValue a) => Value -> m a
exact = either throwError pure . exactEither

exactMaybe :: RecordValue a => Value -> Maybe a
exactMaybe = either (const Nothing) Just . exactEither

instance RecordValue () where
  exactEither (N _) = pure ()
  exactEither _     = throwError NotNull 

instance RecordValue Bool where
  exactEither (B b) = pure b
  exactEither _     = throwError NotBool

instance RecordValue Int where
  exactEither (I i) = pure i
  exactEither _     = throwError NotInt

instance RecordValue Double where
  exactEither (F d) = pure d
  exactEither _     = throwError NotFloat

instance RecordValue Text where
  exactEither (T t) = pure t
  exactEither _     = throwError NotString

instance RecordValue a => RecordValue [a] where
  exactEither (L l) = traverse exactEither l
  exactEither _     = throwError NotList 

instance RecordValue a => RecordValue (Maybe a) where
  exactEither (N _) = pure Nothing
  exactEither x     = Just <$> exactEither x

instance RecordValue (Map Text Value) where
  exactEither (M m) = pure m
  exactEither _     = throwError NotDict

instance RecordValue Node where
  exactEither (S s) = fromStructure s
  exactEither _     = throwError $ Not "Node" 

instance RecordValue Relationship where
  exactEither (S s) = fromStructure s
  exactEither _     = throwError $ Not "Relationship"

instance RecordValue URelationship where
  exactEither (S s) = fromStructure s
  exactEither _     = throwError $ Not "URelationship" 

instance RecordValue Path where
  exactEither (S s) = fromStructure s
  exactEither _     =  throwError $ Not "Path"

at :: Record -> Text -> Maybe Value
at = flip M.lookup
