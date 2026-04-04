{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bolt.Connection.Instances where

import           Database.Bolt.Connection.Type
import           Database.Bolt.Value.Helpers
import           Database.Bolt.Value.Type

import           Control.Monad.Except (MonadError (..))
import           Data.Map.Strict      (Map, empty, fromList, insert, union)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Version         (showVersion)
import           Data.Word            (Word32)
import           GHC.Stack            (HasCallStack)
import           System.Info          (arch, compilerName, compilerVersion, os)

instance ToStructure Request where
  toStructure RequestInit{..}        = Structure sigInit $
    if isV3 initVersion
      then [M $ helloMap agent token initVersion initRouting]
      else [T agent, M $ tokenMap token]
  toStructure RequestRun{..}         = Structure sigRun [T statement, M parameters]
  toStructure RequestRunV3{..}       = Structure sigRun [T statement, M parameters, M extra]
  toStructure RequestReset           = Structure sigReset []
  toStructure RequestAckFailure      = Structure sigAFail []
  toStructure RequestPullAll         = Structure sigPAll []
  toStructure RequestDiscardAll      = Structure sigDAll []
  toStructure RequestGoodbye         = Structure sigGBye []
  toStructure RequestBegin{..}       = Structure sigBegin [M extra]
  toStructure RequestCommit          = Structure sigCommit []
  toStructure RequestRollback        = Structure sigRollback []
  toStructure RequestLogon{..}       = Structure sigLogon [M (tokenMap logonToken)]
  toStructure RequestLogoff          = Structure sigLogoff []
  toStructure RequestTelemetry{..}   = Structure sigTelemetry [I telemetryApi]
  toStructure RequestPull{..}        = Structure sigPAll [M pullExtra]
  toStructure RequestDiscard{..}     = Structure sigDAll [M discardExtra]
  toStructure RequestRoute{..}       = Structure sigRoute [M routeContext, L (map T routeBookmarks), M routeExtra]

instance FromStructure Response where
  fromStructure Structure{..}
    | signature == sigSucc = ResponseSuccess <$> extractMap fields
    | signature == sigRecs = pure $ ResponseRecord (removeExtList fields)
    | signature == sigIgn  = pure ResponseIgnored
    | signature == sigFail = ResponseFailure <$> extractMap fields
    | otherwise            = throwError $ Not "Response"
    where removeExtList :: HasCallStack => [Value] -> [Value]
          removeExtList [L x] = x
          removeExtList _     = error "Record must contain only a singleton list"

-- Response check functions

isSuccess :: Response -> Bool
isSuccess (ResponseSuccess _) = True
isSuccess _                   = False

isFailure :: Response -> Bool
isFailure (ResponseFailure _) = True
isFailure _                   = False

-- Helper functions

createInit :: BoltCfg -> Word32 -> Maybe (Map Text Value) -> Request
createInit BoltCfg{..} serverVer mRouting = RequestInit userAgent
                                               AuthToken { scheme      = authType
                                                         , principal   = user
                                                         , credentials = password
                                                         }
                                               serverVer
                                               mRouting

createAuthToken :: BoltCfg -> AuthToken
createAuthToken BoltCfg{..} = AuthToken { scheme      = authType
                                        , principal   = user
                                        , credentials = password
                                        }

-- |Build the extras map for a HELLO message.
--
-- * BOLT v3:: includes @user_agent@ and inline authentication credentials.
-- * BOLT v5.6+: includes @user_agent@ and optional @routing@ context, but omits
--   credentials (authentication is handled by a separate LOGON message),
--   and additionally includes @bolt_agent@ with a structured product identifier.
helloMap :: Text -> AuthToken -> Word32 -> Maybe (Map Text Value) -> Map Text Value
helloMap userAgent authToken serverVersion mRouting
  | isV5_6 serverVersion =
      let base = fromList
            [ "user_agent" =: userAgent
            , "bolt_agent" =: (fromList
                [ "product" =: userAgent
                , "platform" =: (arch <> "-" <> os)
                , "language" =: ("Haskell/2010" :: Text)
                , "language_details" =: (compilerName <> "-" <> showVersion compilerVersion)
                ])
            ]
      in case mRouting of
           Just ctx -> insert "routing" (M ctx) base
           Nothing  -> base
  | otherwise = insert "user_agent" (T userAgent) (tokenMap authToken)

-- |Credentials for @HELLO@ message in BOLT v3 protocol.
tokenMap :: AuthToken -> Map Text Value
tokenMap at = fromList [ "scheme"     =: scheme at
                       , "principal"   =: principal at
                       , "credentials" =: credentials at
                       ]

-- |For BOLT v5.6+: map with @notifications_minimum_severity@ and
-- @notifications_disabled_classifications@ parameters.
notifExtra :: Word32 -> Maybe Text -> [Text] -> Map Text Value
notifExtra ver msev disabled
  | not (isV5_6 ver) = empty
  | otherwise =
      let sevEntry = case msev of
                       Just s  -> fromList ["notifications_minimum_severity" =: s]
                       Nothing -> empty
          disKey   = "notifications_disabled_classifications"
          disEntry = if null disabled then empty
                     else fromList [(disKey, L (map T disabled))]
      in sevEntry `union` disEntry

-- | Build the routing context map from a 'BoltCfg'.
routingContext :: BoltCfg -> Map Text Value
routingContext cfg = fromList ["address" =: (T.pack (host cfg) <> ":" <> T.pack (show (port cfg)))]

-- | Build the @db@ extra map from an optional database name.
dbExtra :: Maybe Text -> Map Text Value
dbExtra = maybe empty (\db -> fromList ["db" =: db])

extractMap :: MonadError UnpackError m => [Value] -> m (Map Text Value)
extractMap [M mp] = pure mp
extractMap _      = throwError NotDict

mkFailure :: Response -> ResponseError
mkFailure ResponseFailure{..} =
  let code = case M.lookup "code" failMap of { Just (T c) -> c; _ -> "<unknown code>" }
      msg  = case M.lookup "message" failMap of { Just (T m) -> m; _ -> "<unknown message>" }
  in  KnownResponseFailure code msg
mkFailure _ = UnknownResponseFailure
