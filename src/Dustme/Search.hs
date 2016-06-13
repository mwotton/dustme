{-# LANGUAGE TupleSections #-}
module Dustme.Search where

import           Control.Monad       (guard)
import           Data.Char           (isSpace)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (sortBy)
import           Data.Maybe          (mapMaybe)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Dustme.Score        (bestMatches, getIndices, matchComparison)
import           Dustme.Types
import           Safe                (headMay)

-- Could be more sophisticated here
isBoundaryChar = isSpace

emptyCache = HM.empty

getResults :: [Text]
           -> SearchCache
           -> Search
           -> (SearchResult, SearchCache)
getResults candidates cache search = case HM.lookup search cache of
  Nothing -> let result = runSearch search candidates in
               (result, HM.insert search result cache)
  Just cached -> (cached, cache)

runSearch :: Search -> [Text] -> SearchResult
runSearch (Search st) candidates =
  sortBy matchComparison $
  mapMaybe (headMay .  bestMatches st) candidates


applyOp ::  [Text] ->  SearchOp -> Search -> SearchResult -> (Search,[Text])
applyOp candidates op (Search st) matches =
  case op of
    AddText t ->  ( Search (st <> t)
                  , map matchText matches)
    Backspace ->  ( Search (T.dropEnd 1 st)
                  , candidates)
    DeleteWord -> ( Search $ T.dropWhileEnd (not . isBoundaryChar) st
                  , candidates)
