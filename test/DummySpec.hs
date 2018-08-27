module DummySpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "forkTable" $ do
    it "works" $ do
      1 `shouldBe` 1

