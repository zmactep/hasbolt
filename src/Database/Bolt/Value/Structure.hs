{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Bolt.Value.Structure where

import Database.Bolt.Value.Type
import Database.Bolt.Value.Helpers

import Data.Text (Text)

instance FromStructure Node where
  fromStructure (Structure sig lst) | sig == sigNode = mkNode lst
                                    | otherwise      = failNode
    where mkNode :: Monad m => [Value] -> m Node
          mkNode [I nid, L vlbls, M prps] = flip (Node nid) prps <$> cnvT vlbls
          mkNode _                        = failNode

          failNode :: Monad m => m Node
          failNode = fail $ show lst ++ " is not a Node value"

instance FromStructure Relationship where
  fromStructure (Structure sig lst) | sig == sigRel = mkRel lst
                                    | otherwise     = failRel
    where mkRel :: Monad m => [Value] -> m Relationship
          mkRel [I rid, I sni, I eni, T rt, M rp] = pure $ Relationship rid sni eni rt rp
          mkRel _                                 = failRel

          failRel :: Monad m => m Relationship
          failRel = fail $ show lst ++ " is not a Relationship value"

instance FromStructure URelationship where
  fromStructure (Structure sig lst) | sig == sigURel = mkURel lst
                                    | otherwise      = failURel
    where mkURel :: Monad m => [Value] -> m URelationship
          mkURel [I rid, T rt, M rp] = pure $ URelationship rid rt rp
          mkURel _                   = failURel

          failURel :: Monad m => m URelationship
          failURel = fail $ show lst ++ " is not an Unbounded Relationship value"

instance FromStructure Path where
  fromStructure (Structure sig lst) | sig == sigPath = mkPath lst
                                    | otherwise      = failPath
    where mkPath :: Monad m => [Value] -> m Path
          mkPath [L vnp, L vrp, L vip] = Path <$> cnvN vnp <*> cnvR vrp <*> cnvI vip
          mkPath _                     = failPath

          failPath :: Monad m => m Path
          failPath = fail $ show lst ++ " is not a Path value"

-- = Helper functions

cnvT :: Monad m => [Value] -> m [Text]
cnvT []       = pure []
cnvT (T x:xs) = (x:) <$> cnvT xs
cnvT (x:_)    = fail $ "Non-text value (" ++ show x ++ ") in text list"

cnvN :: Monad m => [Value] -> m [Node]
cnvN []       = pure []
cnvN (S x:xs) = (:) <$> fromStructure x <*> cnvN xs
cnvN (x:_)    = fail $ "Non-node value (" ++ show x ++ ") in node list"

cnvR :: Monad m => [Value] -> m [URelationship]
cnvR []       = pure []
cnvR (S x:xs) = (:) <$> fromStructure x <*> cnvR xs
cnvR (x:_)    = fail $ "Non-(u)relationship value (" ++ show x ++ ") in (u)relationship list"

cnvI :: Monad m => [Value] -> m [Int]
cnvI []       = pure []
cnvI (I x:xs) = (x:) <$> cnvI xs
cnvI (x:_)    = fail $ "Non-int value (" ++ show x ++ ") in int list"
