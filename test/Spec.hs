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
    applyOp' (AddText "foo") (Search "") (map mkTrivial ["a"])
      `shouldBe` (Search "foo", ["a"])


  it "can apply a complex op" $ do
    applyOp' (AddText "b") (Search "a") (map mkTrivial ["ab"])
      `shouldBe` (Search "ab", ["ab"])

  -- it "can search" $ do
  --   mapM_ (\(needle, haystack, result) ->
  --           bestMatch needle haystack
  --           `shouldBe` result)
  --     [("ab", "ab", Just (Match 1 0 1 "ab"))
  --     ,("foo", "ab", Nothing)
  --     ,("ab", "acb", Just (Match 2 0 2 "acb"))
  --     ,("ab", "atom batman", Just (Match 2 0 2 "acb"))
  --     ]
