{-# LANGUAGE TupleSections #-}
module Selecta.Tree where
import           Data.Char
import           Data.Hashable (Hashable)
import qualified Data.HashSet  as HS
import           Data.List     (groupBy, minimumBy, nub, sortBy)
import           Data.Maybe
import           Data.Ord      (comparing)
import           Debug.Trace   (trace)
import           Safe          (headMay)

startingAt pred s = case dropWhile pred s of
  [] -> []
  (x:xs) -> (x,dropWhile pred xs):startingAt pred (dropWhile (not .pred) xs)

  -- reached the end of the string, nothing to be special.

  -- string of special characters and something to do
--  (uselessString, (x:xs)) -> (x,xs):startingAt pred (dropWhile (not.pred) xs)


-- orderedLists xs = case startingAt isSpace xs of
--   [] -> []
--   (x:xs) -> x:(zipconcat xs

-- -- ordered xs@(_:r) = startingAt isSpace xs:concatMap ordered 3r
startable xs = map fst . filter (isSpace . fst . snd  )  $ zip [0..] $ pairs (' ':xs)

pairs (x:y:xs) = (x,y):pairs (y:xs)
pairs _ = []

indicesOf x ys =  (x,map snd $ filter ((==x) . fst) $ zip ys [0..])
allIndicesOf
  :: (Enum b, Eq a, Num b,
      Hashable a) =>
     [a] -> [a] -> [(a, [b])]
allIndicesOf xs ys = map (`indicesOf` ys) $ HS.toList $ HS.fromList xs

-- need a memoised table indexed on
-- (current index in search string,current index in candidate)
-- value is score to get there

traceshow x y = trace (show x) y
validIndexSubseqs xs ys = go 0 xs
  where
    go _ [] = [([],0)]
    go i (x:xs) =  [ (bestroute:route, cost+newcost)
                   | (route, cost) <- rest
                   , (bestroute,newcost) <-
                           sortBy (comparing snd)
                           $ mapMaybe (scoresib' (headMay route)) theIndexes
                   -- this sortBy _should_ be a no-op:
                   -- we always look at the closest legal
                   -- sibling.

                   ]
      where
        scoresib' a b = (b,) <$> scoresib a b
        scoresib :: Maybe Int -> Int -> Maybe Int
        scoresib Nothing _ = Just 0 -- if the previous character
                             -- doesn't exist, then we are at the
                             -- bottom and have no cost yet

        scoresib  (Just after) before
          | after <= before = Nothing
          | before `elem` startableIndices   = Just 1
          | otherwise        = Just (after - before)
--        filterOK sibling [] = True
--        filterOK sibling (x:_) = x >= sibling

        rest = traceshow ("rest",earliestIndex+1) $ go (earliestIndex + 1) xs

        earliestIndex :: Int
        earliestIndex = minimum theIndexes
        theIndexes = fromJust $ lookup x allIndices

--        allXIndices = map (go (i+1)) xs
    allIndices = allIndicesOf xs ys
    startableIndices = startable ys
-- allValidSequences xs ys =
--   let indices = zip xs (map (`indicesOf` ys) xs)
--   in go indices
--   where go [] = _
--         go (x:xs) = map (_foo x) (go xs)
-- -- allValidSequences xs ys = [ | (l,i) <- indices

--
--   [ |
--     -- all elements of x
--     x <- xs

--     ]
