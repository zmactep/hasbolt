{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt2.Connection.Protocol where

import           Database.Bolt2.Connection.Type
import           Database.Bolt2.Value.Helpers
import           Database.Bolt2.Value.Type

import           Data.Map.Strict                (Map (..), fromList)
import           Data.Text                      (Text)

convertRequest :: Request -> Structure
convertRequest (RequestInit agent token) = Structure sigInit [T agent, M $ tokenMap token]
convertRequest (RequestRun stmt params)  = Structure sigRun [T stmt, M params]
convertRequest RequestReset              = Structure sigReset []
convertRequest RequestAckFailure         = Structure sigAFail []
convertRequest RequestPullAll            = Structure sigPAll []
convertRequest RequestDiscardAll         = Structure sigDAll []

deconvertResponse :: Monad m => Structure -> m Response
deconvertResponse (Structure sig fields) | sig == sigSucc = ResponseSuccess <$> extractMap (head fields)
                                         | sig == sigRecs = return $ ResponseRecord fields
                                         | sig == sigIgn  = ResponseIgnored <$> extractMap (head fields)
                                         | sig == sigFail = ResponseFailure <$> extractMap (head fields)
                                         | otherwise      = fail "Not a Response value"

-- Helper functions

token :: BoltCfg -> AuthToken
token bcfg = AuthToken { scheme      = "basic"
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
