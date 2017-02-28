{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Record where

import           Database.Bolt.Connection.Type
import           Database.Bolt.Connection.Instances
import           Database.Bolt.Value.Structure ()
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
  exact x     = fail $ show x ++ " is not a Null value"

instance RecordValue Bool where
  exact (B b) = return b
  exact x     = fail $ show x ++ " is not a Bool value"

instance RecordValue Int where
  exact (I i) = return i
  exact x     = fail $ show x ++ " is not an Int value"

instance RecordValue Double where
  exact (F d) = return d
  exact x     = fail $ show x ++ " is not a Double value"

instance RecordValue Text where
  exact (T t) = return t
  exact x     = fail $ show x ++ " is not a Text value"

instance RecordValue a => RecordValue [a] where
  exact (L l) = traverse exact l
  exact x     = fail $ show x ++ " is not a List value"

instance RecordValue a => RecordValue (Maybe a) where
  exact (N _) = return Nothing
  exact x     = Just <$> exact x

instance RecordValue (Map Text Value) where
  exact (M m) = return m
  exact x     = fail $ show x ++ " is not a Map value"

instance RecordValue Node where
  exact (S s) = fromStructure s
  exact x     = fail $ show x ++ " is not a Node value"

instance RecordValue Relationship where
  exact (S s) = fromStructure s
  exact x     = fail $ show x ++ " is not a Relationship value"

instance RecordValue URelationship where
  exact (S s) = fromStructure s
  exact x     = fail $ show x ++ " is not a URelationship value"

instance RecordValue Path where
  exact (S s) = fromStructure s
  exact x     = fail $ show x ++ " is not a Path value"

at :: Monad m => Record -> Text -> m Value
at record key = case key `M.lookup` record of
                  Just result -> return result
                  Nothing     -> fail $ "No such key (" ++ show key ++ ") in record"

mkKeys :: Monad m => Response -> m [Text]
mkKeys (ResponseSuccess response) = let mbKeys = exact =<< ("fields" `M.lookup` response)
                                    in return $ fromMaybe [] mbKeys
mkKeys x = mkFailure x

mkRecord :: [Text] -> Response -> Record
mkRecord keys = M.fromList . zip keys . recsList
