{-# LANGUAGE ScopedTypeVariables #-}
module Selecta.Score where
import           Data.Char          (isSpace, toLower)
import           Data.List          (minimumBy, sortBy)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord           (comparing)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text, pack)
import qualified Data.Text          as T
import           Prelude            hiding ((!!))
import           Safe
import           Selecta.Types

type Score = Int
type Position = Int

getIndices :: Text -> Map Char (Set Int)
getIndices =
  snd . T.foldl'
  (\(i::Int,dict) c ->
      (i+1, Map.insertWith Set.union (toLower c) (Set.singleton i) dict))
  (0, Map.empty)

matchComparison m1 m2 =
  case compare (matchScore m1) (matchScore m2) of
    EQ -> compare (matchStart m1) (matchStart m2)
    x -> x

mkMatch :: Text -> ([Int], Int) -> Maybe Match
mkMatch _ ([],_) = Nothing
mkMatch t (xs,cost) = Just $ Match cost (head xs) (last xs) t

bestMatches :: Text -> Text -> [Match] -- [([Int], Int)]
bestMatches t keys =
    sortBy matchComparison
  $ mapMaybe (mkMatch keys . (\(p,_) -> (reverse p, scorePath p)))
  $ T.foldl' search [([],0)] t
  where
    dict = getIndices keys

    initials :: Set Int
    -- we add 1 because we want the value _after_ whitespace.
    initials = Set.unions . map (Set.map (+1) . snd)  . Map.toList
             $ Map.filterWithKey (\k _ -> isSpace k)  dict

    scorePath :: [Int] -> Int
    scorePath [] = 10000
    scorePath [_] = 0
    scorePath (x:y:xs)
      | Set.member y initials = 1 + scorePath (y:xs)
      | otherwise             = x - y + scorePath (y:xs)

    search :: [([Int], Int)] -> Char -> [([Int], Int)]
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
