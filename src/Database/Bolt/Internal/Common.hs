module Database.Bolt.Internal.Common where

import           Control.Applicative (liftA2)
import           Data.Binary
import           Data.Bits


inRange :: Ord a => (a, a) -> a -> Bool
inRange (low, up) x = low <= x && x < up

isTinyInt :: Integral a => a -> Bool
isTinyInt = inRange (-2^4, 2^7 - 1)

isIntX :: Integral x => x -> x -> Bool
isIntX p = inRange (-2^(p-1), 2^(p-1) - 1)

isTinyWord :: Word8 -> Bool
isTinyWord = liftA2 (||) (< 128) (>= 240)

isTinyText :: Word8 -> Bool
isTinyText = liftA2 (&&) (< 144) (>= 128)

isTinyList :: Word8 -> Bool
isTinyList = liftA2 (&&) (< 160) (>= 144)

isTinyDict :: Word8 -> Bool
isTinyDict = liftA2 (&&) (< 176) (>= 160)

convertToInt :: Integral a => a -> Int
convertToInt = fromIntegral

getSize :: Word8 -> Int
getSize x = fromIntegral $ x .&. 15
