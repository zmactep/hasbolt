{-# LANGUAGE OverloadedStrings #-}

import Data.Hex
import Test.Hspec
import Test.QuickCheck
import Database.Bolt.PackStream

main :: IO ()
main = hspec $
         describe "PackStream" $ do
           it "packs integers correct" $ do
             hex (pack (1::Int)) `shouldBe` "01"
             hex (pack (1234::Int)) `shouldBe` "C904D2"
           it "packs doubles correct" $ do
             hex (pack (6.283185307179586::Double)) `shouldBe` "C1401921FB54442D18"
             return ()
