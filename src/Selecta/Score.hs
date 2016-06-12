{-# LANGUAGE ScopedTypeVariables #-}
module Selecta.Score where
import           Data.Char          (isSpace, toLower)
import           Data.List          (minimumBy, sortBy)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (mapMaybe)
import           Data.Maybe         (listToMaybe)
import           Data.Maybe         (fromMaybe)
import           Data.Ord           (comparing)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text, pack)
import qualified Data.Text          as T
import           Debug.Trace
import           Debug.Trace        (trace)
import           Prelude            hiding ((!!))
import           Safe
import           Selecta.Types
-- headMay [] = Nothing
-- headMay (x:_) = Just x


data SearchTree a
  = Tree [(a, SearchTree a)]
  | Leaf

type Score = Int
type Position = Int

getIndices :: Text -> Map Char (Set Int)
getIndices =
  snd . T.foldl'
  (\(i::Int,dict) c ->
      (i+1, Map.insertWith Set.union (toLower c) (Set.singleton i) dict))
  (0, Map.empty)


bestMatches :: Text -> Map Char (Set Int) -> [([Int], Int)]
bestMatches t dict =
  sortBy (comparing snd)
  $ map (\(p,_) -> (reverse p, scorePath p))
  $ T.foldl' search ([([],0)]) t
  where

    initials :: Set Int
    -- we add 1 because we want the value _after_ whitespace.
    initials = Set.unions . map ((Set.map (+1)) . snd)  . Map.toList $ Map.filterWithKey (\k _ -> isSpace k)  dict

    scorePath :: [Int] -> Int
    scorePath [] = 10000
    scorePath [_] = 0
    scorePath (x:y:xs)
      | Set.member y initials = 1 + scorePath (y:xs)
      | otherwise             = x - y + scorePath (y:xs)

    search :: [([Int], Int)] -> Char -> [([Int], Int)]
          -- here we may have to do recurse.
    search paths c =
      concatMap
      (\(path, earliest) ->
         let next = Set.toList $ okPaths earliest continuations
         in map (\j -> (j:path, j+1)) next
      ) paths
      where continuations = fromMaybe Set.empty $ Map.lookup (toLower c) dict

okPaths :: Ord a => a -> Set a -> Set a
okPaths x xs = case Set.splitMember x xs of
  (_,True,b) -> Set.insert x b
  (_,_,b)    -> b

-- score isBoundary     (s1,e1) s   =
--   let deflt = e1-s1 in
--     case atMay s (length s - 1)  of
--       Nothing -> deflt
--       Just x -> if isBoundary x then 1 else deflt

-- charBoundary x = x==' '
