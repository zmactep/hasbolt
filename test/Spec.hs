{-# LANGUAGE OverloadedStrings #-}

import           Data.Hex
import           Data.Map                 (Map (..))
import qualified Data.Map                 as M (empty, fromList)
import           Data.Text                (Text)
import qualified Data.Text                as T (pack)
import           Database.Bolt.PackStream
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $
         describe "PackStream" $ do
           it "packs integers correct" $ do
             hex (pack (1::Int)) `shouldBe` "01"
             hex (pack (42::Int)) `shouldBe` "2A"
             hex (pack (1234::Int)) `shouldBe` "C904D2"
           it "packs doubles correct" $ do
             hex (pack (6.283185307179586::Double)) `shouldBe` "C1401921FB54442D18"
             hex (pack (-1.1::Double)) `shouldBe` "C1BFF199999999999A"
           it "packs booleans correct" $ do
             hex (pack False) `shouldBe` "C2"
             hex (pack True) `shouldBe` "C3"
           it "packs strings correct" $ do
             hex (pack $ T.pack "") `shouldBe` "80"
             hex (pack $ T.pack "A") `shouldBe` "8141"
             hex (pack $ T.pack "Größenmaßstäbe") `shouldBe` "D0124772C3B6C39F656E6D61C39F7374C3A46265"
             hex (pack $ T.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ") `shouldBe` "D01A4142434445464748494A4B4C4D4E4F505152535455565758595A"
           it "packs lists correct" $ do
             hex (pack ([]::[Int])) `shouldBe` "90"
             hex (pack ([1,2,3]::[Int])) `shouldBe` "93010203"
             hex (pack ([1..40]::[Int])) `shouldBe` "D4280102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728"
           it "packs dicts correct" $ do
             hex (pack (M.empty :: Map Text ())) `shouldBe` "A0"
             hex (pack (M.fromList [(T.pack "one", T.pack "eins")])) `shouldBe` "A1836F6E658465696E73"
           it "packs () correct" $ do
             hex (pack ()) `shouldBe` "C0"
             return ()
