{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bolt.Connection.Instances where

import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type

import           Data.Map.Strict                (Map, fromList, empty, (!))
import           Data.Text                      (Text)

instance ToStructure Request where
  toStructure RequestInit{..} = Structure sigInit [T agent, M $ tokenMap token]
  toStructure RequestRun{..}  = Structure sigRun [T statement, M parameters]
  toStructure RequestReset              = Structure sigReset []
  toStructure RequestAckFailure         = Structure sigAFail []
  toStructure RequestPullAll            = Structure sigPAll []
  toStructure RequestDiscardAll         = Structure sigDAll []

instance FromStructure Response where
  fromStructure Structure{..}
    | signature == sigSucc = ResponseSuccess <$> extractMap (head fields)
    | signature == sigRecs = pure $ ResponseRecord (removeExtList fields)
    | signature == sigIgn  = ResponseIgnored <$> extractMap (head fields)
    | signature == sigFail = ResponseFailure <$> extractMap (head fields)
    | otherwise            = fail "Not a Response value"
    where removeExtList :: [Value] -> [Value]
          removeExtList [L x] = x
          removeExtList _     = error "Record must contain only a singleton list"

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
createInit BoltCfg{..} = RequestInit userAgent
                                     AuthToken { scheme      = "basic"
                                               , principal   = user
                                               , credentials = password
                                               }

createRun :: Text -> Request
createRun stmt = RequestRun stmt empty


tokenMap :: AuthToken -> Map Text Value
tokenMap at = fromList [ ("scheme",      T $ scheme at)
                       , ("principal",   T $ principal at)
                       , ("credentials", T $ credentials at)
                       ]

extractMap :: Monad m => Value -> m (Map Text Value)
extractMap (M mp) = pure mp
extractMap _      = fail "Not a Dict value"

mkFailure :: Monad m => Response -> m a
mkFailure ResponseFailure{..} =
  let (T code) = failMap ! "code"
      (T msg)  = failMap ! "message"
  in fail $ "code: " ++ show code ++ ", message: " ++ show msg
mkFailure _ = fail "Unknown error"
