{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Record where

import           Database.Bolt.Connection.Instances
import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Structure
import           Database.Bolt.Value.Type

import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)

-- |Result type for query requests
type Record = Map Text Value

-- |Get exact type from Value
class RecordValue a where
  exact :: Monad m => Value -> m a

instance RecordValue () where
  exact (N _) = return ()
  exact _     = fail "Not a Null value"

instance RecordValue Bool where
  exact (B b) = return b
  exact _     = fail "Not a Bool value"

instance RecordValue Int where
  exact (I i) = return i
  exact _     = fail "Not an Int value"

instance RecordValue Double where
  exact (F d) = return d
  exact _     = fail "Not a Double value"

instance RecordValue Text where
  exact (T t) = return t
  exact _     = fail "Not a Text value"

instance RecordValue a => RecordValue [a] where
  exact (L l) = traverse exact l
  exact _     = fail "Not a List value"

instance RecordValue a => RecordValue (Maybe a) where
  exact (N _) = return $ Nothing
  exact x     = Just <$> exact x

instance RecordValue (Map Text Value) where
  exact (M m) = return m
  exact _     = fail "Not a Map value"

instance RecordValue Node where
  exact (S s) = fromStructure s
  exact _     = fail "Not a Node value"

instance RecordValue Relationship where
  exact (S s) = fromStructure s
  exact _     = fail "Not a Relationship value"

instance RecordValue URelationship where
  exact (S s) = fromStructure s
  exact _     = fail "Not a URelationship value"

instance RecordValue Path where
  exact (S s) = fromStructure s
  exact _     = fail "Not a Path value"

at :: Monad m => Record -> Text -> m Value
at record key = case key `M.lookup` record of
                  Just result -> return result
                  Nothing     -> fail "No such key in record"

toRecords :: [Response] -> [Record]
toRecords (ResponseSuccess response:rest) =
  let keys :: [Text]
      keys = fromMaybe [] $ exact =<< ("fields" `M.lookup` response)
  in map (M.fromList . zip keys . recsList) $ init rest
toRecords  _ = []
