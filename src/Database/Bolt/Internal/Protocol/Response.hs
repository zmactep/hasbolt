{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Internal.Protocol.Response
    ( Response (..)
    , UnpackStream (..)
    , isSuccess, isFailure
    , isRecord, isIgnored
    ) where

import           Control.Monad                           (replicateM)
import           Data.Map.Strict                         (Map (..), (!))
import           Data.Text                               (Text)
import           Data.Word                               (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number    (unpackW16, unpackW8)
import           Database.Bolt.Internal.Unpack.Structure (unpackSizeAndSig)
import           Database.Bolt.NeoValue                  (NeoValue (..),
                                                          UnpackStream (..),
                                                          UnpackT (..))

data Failure = Failure { failureCode    :: Text
                       , failureMessage :: Text
                       }
  deriving (Eq, Show)

data Response = ResponseSuccess [NeoValue]
              | ResponseRecord [NeoValue]
              | ResponseIgnored [NeoValue]
              | ResponseFailure [NeoValue]
  deriving (Eq, Show)

instance UnpackStream Response where
  unpack = do (size, sig) <- unpackSizeAndSig
              unpackBySignature sig size

unpackBySignature :: Monad m => Word8 -> Int -> UnpackT m Response
unpackBySignature w size | w == 112  = ResponseSuccess <$> unpackBySize
                         | w == 113  = ResponseRecord  <$> unpackBySize
                         | w == 126  = ResponseIgnored <$> unpackBySize
                         | w == 127  = ResponseFailure <$> unpackBySize
                         | otherwise = fail "Not recognisable response"
  where unpackBySize = replicateM size unpack

isSuccess :: Response -> Bool
isSuccess (ResponseSuccess _) = True
isSuccess _                   = False

isFailure :: Response -> Bool
isFailure (ResponseFailure _) = True
isFailure _                   = False

isRecord :: Response -> Bool
isRecord (ResponseRecord _) = True
isRecord _                  = False

isIgnored :: Response -> Bool
isIgnored (ResponseIgnored _) = True
isIgnored _                   = False

parseFailure :: Response -> Failure
parseFailure (ResponseFailure [mp]) = Failure (fromNVT $ mpM ! "code") (fromNVT $ mpM ! "message")
    where mpM = fromNVM mp
          fromNVM :: NeoValue -> Map Text NeoValue
          fromNVM (M mp) = mp
          fromNVM _      = error "Cannot extract Text from non-text value"
          fromNVT :: NeoValue -> Text
          fromNVT (T txt) = txt
          fromNVT _       = error "Cannot extract Text from non-text value"
