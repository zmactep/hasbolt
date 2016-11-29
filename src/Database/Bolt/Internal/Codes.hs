module Database.Bolt.Internal.Codes where

import Data.Word

-- Null

nullCode :: Word8
nullCode = 192

-- Bool

falseCode :: Word8
falseCode = 194

trueCode :: Word8
trueCode = 195

-- Numbers

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

-- Text

textConst :: Word8
textConst = 128

text8Code :: Word8
text8Code = 208

text16Code :: Word8
text16Code = 209

text32Code :: Word8
text32Code = 210

-- List

listConst :: Word8
listConst = 144

list8Code :: Word8
list8Code = 212

list16Code :: Word8
list16Code = 213

list32Code :: Word8
list32Code = 214

-- Dict

dictConst :: Word8
dictConst = 160

dict8Code :: Word8
dict8Code = 216

dict16Code :: Word8
dict16Code = 217

dict32Code :: Word8
dict32Code = 218
