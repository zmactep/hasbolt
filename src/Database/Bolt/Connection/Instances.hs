{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Connection.Instances where

import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type

import           Data.Map.Strict                (Map (..), fromList, empty)
import           Data.Text                      (Text)

instance Structable Request where
  toStructure (RequestInit agent token) = Structure sigInit [T agent, M $ tokenMap token]
  toStructure (RequestRun stmt params)  = Structure sigRun [T stmt, M params]
  toStructure RequestReset              = Structure sigReset []
  toStructure RequestAckFailure         = Structure sigAFail []
  toStructure RequestPullAll            = Structure sigPAll []
  toStructure RequestDiscardAll         = Structure sigDAll []

  fromStructure = undefined

instance Structable Response where
  toStructure = undefined

  fromStructure (Structure sig fields) | sig == sigSucc = ResponseSuccess <$> extractMap (head fields)
                                       | sig == sigRecs = return $ ResponseRecord (removeExtList fields)
                                       | sig == sigIgn  = ResponseIgnored <$> extractMap (head fields)
                                       | sig == sigFail = ResponseFailure <$> extractMap (head fields)
                                       | otherwise      = fail "Not a Response value"
    where removeExtList :: [Value] -> [Value]
          removeExtList [L x] = x
          removeExtList _     = error "Record must contain only on value list"

-- Response check functions

isSuccess :: Response -> Bool
isSuccess (ResponseSuccess _) = True
isSuccess _                   = False

isFailure :: Response -> Bool
isFailure (ResponseFailure _) = True
isFailure _                   = False

isIgnored :: Response -> Bool
isIgnored (ResponseIgnored _) = True
isIgnored _                   = False

isRecord :: Response -> Bool
isRecord (ResponseRecord _) = True
isRecord _                  = False

-- Helper functions

createInit :: BoltCfg -> Request
createInit bcfg = RequestInit (userAgent bcfg) (tokenOf bcfg)

createRun :: Text -> Request
createRun stmt = RequestRun stmt empty

tokenOf :: BoltCfg -> AuthToken
tokenOf bcfg = AuthToken { scheme      = "basic"
                         , principal   = user bcfg
                         , credentials = password bcfg
                         }

tokenMap :: AuthToken -> Map Text Value
tokenMap at = fromList [ ("scheme",      T $ scheme at)
                       , ("principal",   T $ principal at)
                       , ("credentials", T $ credentials at)
                       ]

extractMap :: Monad m => Value -> m (Map Text Value)
extractMap (M mp) = return mp
extractMap _      = fail "Not a Dict value"
