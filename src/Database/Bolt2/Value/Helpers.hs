module Database.Bolt2.Value.Helpers where

import           Control.Applicative (liftA2, liftA3)
import           Data.Bits           ((.&.))
import           Data.Word           (Word8)

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
           return $ x || y || z

isDouble :: Word8 -> Bool
isDouble = (== doubleCode)

isDict :: Word8 -> Bool
isDict = do x <- liftA2 (||) (== dict8Code) (== dict16Code)
            y <- liftA2 (||) (== dict32Code) isTinyDict
            return $ x || y

isText :: Word8 -> Bool
isText = do x <- liftA2 (||) (== text8Code) (== text16Code)
            y <- liftA2 (||) (== text32Code) isTinyText
            return $ x || y

isList :: Word8 -> Bool
isList = do x <- liftA2 (||) (== list8Code) (== list16Code)
            y <- liftA2 (||) (== list32Code) isTinyList
            return $ x || y

isStruct :: Word8 -> Bool
isStruct = liftA3 (\x y z -> x || y || z) (== struct8Code) (== struct16Code) isTinyStruct

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

sigInit :: Word8
sigInit = 1

sigRun :: Word8
sigRun = 16

sigAFail :: Word8
sigAFail = 14

sigReset :: Word8
sigReset = 15

sigDAll :: Word8
sigDAll = 47

sigPAll :: Word8
sigPAll = 63

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
