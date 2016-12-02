module Database.Bolt.Internal.Codes where

import           Control.Applicative (liftA2, liftA3)
import           Data.Word

import Database.Bolt.Internal.Common

-- Null

nullCode :: Word8
nullCode = 192

isNull :: Word8 -> Bool
isNull = (== nullCode)

-- Bool

falseCode :: Word8
falseCode = 194

trueCode :: Word8
trueCode = 195

isBool :: Word8 -> Bool
isBool = liftA2 (||) (== trueCode) (== falseCode)

-- Numbers

int8Code :: Word8
int8Code = 200

int16Code :: Word8
int16Code = 201

int32Code :: Word8
int32Code = 202

int64Code :: Word8
int64Code = 203

isInt :: Word8 -> Bool
isInt = do x <- liftA2 (||) (== int8Code) (== int16Code)
           y <- liftA2 (||) (== int32Code) (== int64Code)
           z <- isTinyWord
           return $ x || y || z

doubleCode :: Word8
doubleCode = 193

isDouble :: Word8 -> Bool
isDouble = (== doubleCode)

-- Text

textConst :: Word8
textConst = 128

text8Code :: Word8
text8Code = 208

text16Code :: Word8
text16Code = 209

text32Code :: Word8
text32Code = 210

isText :: Word8 -> Bool
isText = do x <- liftA2 (||) (== text8Code) (== text16Code)
            y <- liftA2 (||) (== text32Code) isTinyText
            return $ x || y

-- List

listConst :: Word8
listConst = 144

list8Code :: Word8
list8Code = 212

list16Code :: Word8
list16Code = 213

list32Code :: Word8
list32Code = 214

isList :: Word8 -> Bool
isList = do x <- liftA2 (||) (== list8Code) (== list16Code)
            y <- liftA2 (||) (== list32Code) isTinyList
            return $ x || y

-- Dict

dictConst :: Word8
dictConst = 160

dict8Code :: Word8
dict8Code = 216

dict16Code :: Word8
dict16Code = 217

dict32Code :: Word8
dict32Code = 218

isDict :: Word8 -> Bool
isDict = do x <- liftA2 (||) (== dict8Code) (== dict16Code)
            y <- liftA2 (||) (== dict32Code) isTinyDict
            return $ x || y

-- Structure

structConst :: Word8
structConst = 176

struct8Code :: Word8
struct8Code = 220

struct16Code :: Word8
struct16Code = 221

isStruct :: Word8 -> Bool
isStruct = liftA3 (\x y z -> x || y || z) (== struct8Code) (== struct16Code) isTinyStruct
