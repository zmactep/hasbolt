module Database.Bolt.Internal.Protocol.Response
    ( Response (..)
    , UnpackStream (..)
    , isSuccess, isFailure
    , isRecord, isIgnored
    ) where

import Control.Monad (replicateM)
import           Data.Word                            (Word8)
import           Database.Bolt.Internal.Codes
import           Database.Bolt.Internal.Common
import           Database.Bolt.Internal.Unpack.Number (unpackW16, unpackW8)
import           Database.Bolt.UnpackStream           (UnpackStream (..),
                                                       UnpackT (..), Unpacked)

data Response = ResponseSuccess [Unpacked]
              | ResponseRecord [Unpacked]
              | ResponseIgnored [Unpacked]
              | ResponseFailure [Unpacked]
  deriving (Eq, Show)

instance UnpackStream Response where
  unpack = do marker <- unpackW8
              size <- getStructSize marker
              sig <- unpackW8
              unpackBySignature sig size

unpackBySignature :: Monad m => Word8 -> Int -> UnpackT m Response
unpackBySignature w size | w == 112  = ResponseSuccess <$> unpackBySize
                         | w == 113  = ResponseRecord  <$> unpackBySize
                         | w == 126  = ResponseIgnored <$> unpackBySize
                         | w == 127  = ResponseFailure <$> unpackBySize
                         | otherwise = fail "Not recognisable response"
  where unpackBySize = replicateM size unpack

getStructSize :: Monad m => Word8 -> UnpackT m Int
getStructSize m | isTinyStruct m    = return $ getSize m
                | m == struct8Code  = convertToInt <$> unpackW8
                | m == struct16Code = convertToInt <$> unpackW16
                | otherwise         = fail "Not a structure value"

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
