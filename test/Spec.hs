{-# LANGUAGE OverloadedStrings #-}
import           Selecta
import           Selecta.Score
import           Selecta.Search
import           Selecta.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec = describe "applyOp" $ do
  let applyOp' = applyOp ["a", "ab", "abc", "xyzzy"]
  it "can apply an op" $ do
    applyOp' (AddText "foo") (Search "") (map mkTrivialMatch ["a"])
      `shouldBe` (Search "foo", ["a"])


  it "can apply a complex op" $ do
    applyOp' (AddText "b") (Search "a") (map mkTrivialMatch ["ab"])
      `shouldBe` (Search "ab", ["ab"])
