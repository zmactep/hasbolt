{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Bolt.Value.Structure where

import Database.Bolt.Value.Type
import Database.Bolt.Value.Helpers

import Data.Text (Text)

instance FromStructure Node where
  fromStructure (Structure sig lst) | sig == sigNode = mkNode lst
                                    | otherwise      = failNode
    where mkNode :: Monad m => [Value] -> m Node
          mkNode [I nid, L vlbls, M props] = flip (Node nid) props <$> cnvT vlbls
          mkNode _                         = failNode

          failNode :: Monad m => m Node
          failNode = fail $ show lst ++ " is not a Node value"

instance FromStructure Relationship where
  fromStructure (Structure sig lst) | sig == sigRel = mkRel lst
                                    | otherwise     = failRel
    where mkRel :: Monad m => [Value] -> m Relationship
          mkRel [I rid, I sni, I eni, T rt, M rp] = return $ Relationship rid sni eni rt rp
          mkRel _                                 = failRel

          failRel :: Monad m => m Relationship
          failRel = fail $ show lst ++ " is not a Relationship value"

instance FromStructure URelationship where
  fromStructure (Structure sig lst) | sig == sigURel = mkURel lst
                                    | otherwise      = failURel
    where mkURel :: Monad m => [Value] -> m URelationship
          mkURel [I rid, T rt, M rp] = return $ URelationship rid rt rp
          mkURel _                   = failURel

          failURel :: Monad m => m URelationship
          failURel = fail $ show lst ++ " is not an Unbounded Relationship value"

instance FromStructure Path where
  fromStructure (Structure sig lst) | sig == sigPath = mkPath lst
                                    | otherwise      = failPath
    where mkPath :: Monad m => [Value] -> m Path
          mkPath [L vnp, L vrp, L vip] = do np <- cnvN vnp
                                            rp <- cnvR vrp
                                            ip <- cnvI vip
                                            return $ Path np rp ip
          mkPath _                     = failPath

          failPath :: Monad m => m Path
          failPath = fail $ show lst ++ " is not a Path value"

-- = Helper functions

cnvT :: Monad m => [Value] -> m [Text]
cnvT []       = return []
cnvT (T x:xs) = (x:) <$> cnvT xs
cnvT (x:_)    = fail $ "Non-text value (" ++ show x ++ ") in text list"

cnvN :: Monad m => [Value] -> m [Node]
cnvN []       = return []
cnvN (S x:xs) = do hd <- fromStructure x
                   rest <- cnvN xs
                   return (hd:rest)
cnvN (x:_)    = fail $ "Non-node value (" ++ show x ++ ") in node list"

cnvR :: Monad m => [Value] -> m [URelationship]
cnvR []       = return []
cnvR (S x:xs) = do hd <- fromStructure x
                   rest <- cnvR xs
                   return (hd:rest)
cnvR (x:_)    = fail $ "Non-(u)relationship value (" ++ show x ++ ") in (u)relationship list"

cnvI :: Monad m => [Value] -> m [Int]
cnvI []       = return []
cnvI (I x:xs) = (x:) <$> cnvI xs
cnvI (x:_)    = fail $ "Non-int value (" ++ show x ++ ") in int list"
