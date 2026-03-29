{-# LANGUAGE CPP #-}

module Database.Bolt.Value.Helpers where

import           Control.Applicative (liftA3)
#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           Data.Bits           ((.&.), shiftR)
import           Data.Word           (Word8, Word32)

-- = Checkers

isTinyInt :: Integral a => a -> Bool
isTinyInt = inRange (-16, 128 - 1)

isTinyWord :: Word8 -> Bool
isTinyWord = liftA2 (||) (< textConst) (>= 240)

isTinyText :: Word8 -> Bool
isTinyText = liftA2 (&&) (>= textConst) (< listConst)

isTinyList :: Word8 -> Bool
isTinyList = liftA2 (&&) (>= listConst) (< dictConst)

isTinyDict :: Word8 -> Bool
isTinyDict = liftA2 (&&) (>= dictConst) (< structConst)

isTinyStruct :: Word8 -> Bool
isTinyStruct = liftA2 (&&) (>= structConst) (< nullCode)

isNull :: Word8 -> Bool
isNull = (== nullCode)

isBool :: Word8 -> Bool
isBool = liftA2 (||) (== trueCode) (== falseCode)

isInt :: Word8 -> Bool
isInt = do x <- liftA2 (||) (== int8Code) (== int16Code)
           y <- liftA2 (||) (== int32Code) (== int64Code)
           z <- isTinyWord
           pure $ x || y || z

isDouble :: Word8 -> Bool
isDouble = (== doubleCode)

isDict :: Word8 -> Bool
isDict = do x <- liftA2 (||) (== dict8Code) (== dict16Code)
            y <- liftA2 (||) (== dict32Code) isTinyDict
            pure $ x || y

isText :: Word8 -> Bool
isText = do x <- liftA2 (||) (== text8Code) (== text16Code)
            y <- liftA2 (||) (== text32Code) isTinyText
            pure $ x || y

isList :: Word8 -> Bool
isList = do x <- liftA2 (||) (== list8Code) (== list16Code)
            y <- liftA2 (||) (== list32Code) isTinyList
            pure $ x || y

-- |Checks whether a marker byte indicates a PackStream Bytes value (0xCC, 0xCD, 0xCE).
-- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
isBytes :: Word8 -> Bool
isBytes = liftA3 (\x y z -> x || y || z) (== bytes8Code) (== bytes16Code) (== bytes32Code)

isStruct :: Word8 -> Bool
isStruct = liftA3 (\x y z -> x || y || z) (== struct8Code) (== struct16Code) isTinyStruct

-- Version check functions
--
-- The negotiated BOLT protocol version is stored as a 'Word32' with the major
-- version in the low byte and the minor version in the next byte:
--
-- @
--   version = (minor << 8) | major
-- @
--
-- For example, BOLT 5.3 is stored as @0x0305@.

-- |Checks for BOLT v3+, which introduced Hello\/Goodbye and explicit transactions.
isV3 :: Word32 -> Bool
isV3 v = (v .&. 255) >= 3

-- |Checks for BOLT v5.0+, which introduced separate Hello\/Logon authentication
-- and PULL\/DISCARD with explicit count parameters.
isV5 :: Word32 -> Bool
isV5 = isV5_N 1

versionMinor :: Word32 -> Word32
versionMinor v = (v `shiftR` 8) .&. 0xFF

-- |Checks for BOLT v5.N+ (major > 5, or major == 5 with minor >= N).
isV5_N :: Word32 -> Word32 -> Bool
isV5_N n v = let major = v .&. 0xFF
                 minor = versionMinor v
             in major > 5 || (major == 5 && minor >= n)

-- |Checks for BOLT v4.3+, which introduced the ROUTE message.
isV4_3 :: Word32 -> Bool
isV4_3 v = let major = v .&. 0xFF
               minor = versionMinor v
           in major >= 5 || (major == 4 && minor >= 3)

-- |Checks for BOLT v5.2+, which added notification filtering.
isV5_2 :: Word32 -> Bool
isV5_2 = isV5_N 2

-- |Checks for BOLT v5.3+, which added @bolt_agent@ in HELLO.
isV5_3 :: Word32 -> Bool
isV5_3 = isV5_N 3

-- |Checks for BOLT v5.6+, which renamed notification categories to classifications.
isV5_6 :: Word32 -> Bool
isV5_6 = isV5_N 6

-- = Constants

-- == Null

nullCode :: Word8
nullCode = 192

-- == Bool

falseCode :: Word8
falseCode = 194

trueCode :: Word8
trueCode = 195

-- == Numbers

int8Code :: Word8
int8Code = 200

int16Code :: Word8
int16Code = 201

int32Code :: Word8
int32Code = 202

int64Code :: Word8
int64Code = 203

doubleCode :: Word8
doubleCode = 193

-- == Text

textConst :: Word8
textConst = 128

text8Code :: Word8
text8Code = 208

text16Code :: Word8
text16Code = 209

text32Code :: Word8
text32Code = 210

-- == List

listConst :: Word8
listConst = 144

list8Code :: Word8
list8Code = 212

list16Code :: Word8
list16Code = 213

list32Code :: Word8
list32Code = 214

-- == Dict

dictConst :: Word8
dictConst = 160

dict8Code :: Word8
dict8Code = 216

dict16Code :: Word8
dict16Code = 217

dict32Code :: Word8
dict32Code = 218

-- == Bytes

-- |Marker for Bytes8: up to 2^8-1 bytes. PackStream marker 0xCC.
-- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
bytes8Code :: Word8
bytes8Code = 0xCC

-- |Marker for Bytes16: up to 2^16-1 bytes. PackStream marker 0xCD.
-- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
bytes16Code :: Word8
bytes16Code = 0xCD

-- |Marker for Bytes32: up to 2^32-1 bytes. PackStream marker 0xCE.
-- See: https://neo4j.com/docs/bolt/current/packstream/#data-type-bytes
bytes32Code :: Word8
bytes32Code = 0xCE

-- == Structure

structConst :: Word8
structConst = 176

struct8Code :: Word8
struct8Code = 220

struct16Code :: Word8
struct16Code = 221

-- == Neo4j subject signatures

sigNode :: Word8
sigNode = 78

sigRel :: Word8
sigRel = 82

sigURel :: Word8
sigURel = 114

sigPath :: Word8
sigPath = 80

-- == BOLT requests signatures
-- https://neo4j.com/docs/bolt/current/bolt/message/

-- @INIT@ in v1 & v2, @HELLO@ in v3.
sigInit :: Word8
sigInit = 0x01

-- @RUN@.
sigRun :: Word8
sigRun = 0x10

-- @ACK_FAILURE@, removed in v3.
sigAFail :: Word8
sigAFail = 0x0e

-- @RESET@
sigReset :: Word8
sigReset = 0x0f

-- @DISCARD_ALL@ in v1 & v2, @DISCARD@ in v3.
sigDAll :: Word8
sigDAll = 0x2f

-- @PULL_ALL@ in v1 & v2, @PULL@ in v3.
sigPAll :: Word8
sigPAll = 0x3f

-- @GOODBYE@, introduced in v3.
sigGBye :: Word8
sigGBye = 0x02

-- @LOGON@, introduced in v5.1.
sigLogon :: Word8
sigLogon = 0x6A

-- @LOGOFF@, introduced in v5.1.
sigLogoff :: Word8
sigLogoff = 0x6B

-- @TELEMETRY@, introduced in v5.4.
sigTelemetry :: Word8
sigTelemetry = 0x54

-- @BEGIN@, introduced in v3.
sigBegin :: Word8
sigBegin = 0x11

-- @COMMIT@, introduced in v3.
sigCommit :: Word8
sigCommit = 0x12

-- @ROLLBACK@, introduced in v3.
sigRollback :: Word8
sigRollback = 0x13

-- @ROUTE@, introduced in v4.3.
sigRoute :: Word8
sigRoute = 0x66

-- == BOLT responses signatures

sigSucc :: Word8
sigSucc = 112

sigFail :: Word8
sigFail = 127

sigRecs :: Word8
sigRecs = 113

sigIgn :: Word8
sigIgn = 126

-- = Other helpers

toInt :: Integral a => a -> Int
toInt = fromIntegral

getSize :: Word8 -> Int
getSize x = fromIntegral $ x .&. 15

inRange :: Ord a => (a, a) -> a -> Bool
inRange (low, up) x = low <= x && x < up

isIntX :: Integral x => x -> x -> Bool
isIntX p = inRange (-2^(p-1), 2^(p-1) - 1)
