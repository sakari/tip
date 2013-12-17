module TypingSpec (main, spec) where

import Test.Hspec
import Language.Tip.Quote
import Language.Tip.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test it good" $ do
    it "is tested real good" $ do
      "a" `shouldBe` "b"
