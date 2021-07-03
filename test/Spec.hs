{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative      ((<$>))
import           Data.Binary.Put          (runPut)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (fromStrict, toStrict)
import           Hex
import           Data.Map                 (Map)
import qualified Data.Map                 as M (empty, fromList)
import           Data.Text                (Text)
import qualified Data.Text                as T (pack)
import           Test.Hspec
#if !MIN_VERSION_base(4, 13, 0)
import           Control.Monad.Fail       (MonadFail)
#endif

import Database.Bolt ()
import Database.Bolt.Serialization

main :: IO ()
main = hspec $ do
         packStreamTests
         unpackStreamTests

pack' :: BoltValue a => a -> ByteString
pack' = toStrict . runPut . pack

unpackStreamTests :: Spec
unpackStreamTests =
  describe "Unpack" $ do
    it "unpacks integers correct" $ do
      u1 <- prepareData "01" >>= unpackF :: IO Int
      u1 `shouldBe` 1
      u42 <- prepareData "2A" >>= unpackF :: IO Int
      u42 `shouldBe` 42
      u1234 <- prepareData "C904D2" >>= unpackF :: IO Int
      u1234 `shouldBe` 1234
    it "unpacks doubles correct" $ do
      u6d <- prepareData "C1401921FB54442D18" >>= unpackF :: IO Double
      u6d `shouldBe` 6.283185307179586
      um1d <- prepareData "C1BFF199999999999A" >>= unpackF :: IO Double
      um1d `shouldBe` (-1.1)
    it "unpacks booleans correct" $ do
      uF <- prepareData "C2" >>= unpackF :: IO Bool
      uF `shouldBe` False
      uT <- prepareData "C3" >>= unpackF :: IO Bool
      uT `shouldBe` True
    it "unpacks strings correct" $ do
      usE <- prepareData "80" >>= unpackF :: IO Text
      usE `shouldBe` T.pack ""
      usA <- prepareData "8141" >>= unpackF :: IO Text
      usA `shouldBe` T.pack "A"
      usU <- prepareData "D0124772C3B6C39F656E6D61C39F7374C3A46265" >>= unpackF :: IO Text
      usU `shouldBe` T.pack "Größenmaßstäbe"
    it "unpacks lists correct" $ do
      ulE <- prepareData "90" >>= unpackF :: IO [Int]
      ulE `shouldBe` []
      ulI <- prepareData "93010203" >>= unpackF :: IO [Int]
      ulI `shouldBe` [1,2,3]
      ulL <- prepareData "D4280102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728" >>= unpackF :: IO [Int]
      ulL `shouldBe` [1..40]
    it "unpacks dicts correct" $ do
      udE <- prepareData "A0" >>= unpackF :: IO (Map Text ())
      udE `shouldBe` M.fromList []
      udS <- prepareData "A1836F6E658465696E73" >>= unpackF :: IO (Map Text Text)
      udS `shouldBe` M.fromList [(T.pack "one", T.pack "eins")]
    it "unpacks () correct" $ do
      uN <- prepareData "C0" >>= unpackF :: IO ()
      uN `shouldBe` ()

packStreamTests :: Spec
packStreamTests =
  describe "Pack" $ do
    it "packs integers correct" $ do
      hex (pack' (1::Int)) `shouldBe` "01"
      hex (pack' (42::Int)) `shouldBe` "2A"
      hex (pack' (1234::Int)) `shouldBe` "C904D2"
    it "packs doubles correct" $ do
      hex (pack' (6.283185307179586::Double)) `shouldBe` "C1401921FB54442D18"
      hex (pack' (-1.1::Double)) `shouldBe` "C1BFF199999999999A"
    it "packs booleans correct" $ do
      hex (pack' False) `shouldBe` "C2"
      hex (pack' True) `shouldBe` "C3"
    it "packs strings correct" $ do
      hex (pack' $ T.pack "") `shouldBe` "80"
      hex (pack' $ T.pack "A") `shouldBe` "8141"
      hex (pack' $ T.pack "Größenmaßstäbe") `shouldBe` "D0124772C3B6C39F656E6D61C39F7374C3A46265"
      hex (pack' $ T.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ") `shouldBe` "D01A4142434445464748494A4B4C4D4E4F505152535455565758595A"
    it "packs lists correct" $ do
      hex (pack' ([]::[Int])) `shouldBe` "90"
      hex (pack' ([1,2,3]::[Int])) `shouldBe` "93010203"
      hex (pack' ([1..40]::[Int])) `shouldBe` "D4280102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728"
    it "packs dicts correct" $ do
      hex (pack' (M.empty :: Map Text ())) `shouldBe` "A0"
      hex (pack' (M.fromList [(T.pack "one", T.pack "eins")])) `shouldBe` "A1836F6E658465696E73"
    it "packs () correct" $
      hex (pack' ()) `shouldBe` "C0"

prepareData :: MonadFail m => ByteString -> m ByteString
prepareData = (toStrict <$>) . unhex . fromStrict
