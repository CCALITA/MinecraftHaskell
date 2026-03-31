module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MinecraftHaskell" $ do
    it "placeholder: project builds" $ do
      True `shouldBe` True
