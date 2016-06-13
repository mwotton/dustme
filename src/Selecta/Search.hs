{-# LANGUAGE TupleSections #-}
module Selecta.Search where



import           Control.Monad       (guard)
import           Data.Char           (isSpace)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (sortBy)
import qualified Data.List.Extra     as DL
import           Data.Maybe          (mapMaybe)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Debug.Trace
import           Safe                (headMay)
import           Selecta.Score       (bestMatches, getIndices, matchComparison)
import           Selecta.Types

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

--   def selection
--     if @aborted
--       NoSelection
--     else
--       match = best_matches.fetch(@index) { NoSelection }
--       if match == NoSelection
--         match
--       else
--         match.original_choice
--       end
--     end
--   end

--   def down
--     move_cursor(1)
--   end

--   def up
--     move_cursor(-1)
--   end

--   def max_visible_choices
--     [@config.visible_choices, all_matches.count].min
--   end

--   def append_search_string(string)
--     merge(:index => 0,
--           :query => @query + string)
--     .recompute_matches(all_matches)
--   end

--   def backspace
--     merge(:index => 0,
--           :query => @query[0...-1])
--     .recompute_matches
--   end

--   def clear_query
--     merge(:index => 0,
--           :query => "")
--     .recompute_matches
--   end

--   def delete_word
--     merge(:index => 0,
--           :query => @query.sub(/[^ ]* *$/, ""))
--     .recompute_matches
--   end

--   def done
--     merge(:done => true)
--   end

--   def abort
--     merge(:aborted => true)
--   end

--   def recompute_matches(previous_matches=self.original_matches)
--     if self.query.empty?
--       merge(:all_matches => original_matches,
--             :best_matches => original_matches)
--     else
--       all_matches = recompute_all_matches(previous_matches)
--       best_matches = recompute_best_matches(all_matches)
--       merge(:all_matches => all_matches, :best_matches => best_matches)
--     end
--   end

--   private

--   def recompute_all_matches(previous_matches)
--     query = self.query.downcase
--     query_chars = query.chars.to_a

--     matches = previous_matches.map do |match|
--       choice = match.choice
--       score, range = Score.score(choice, query_chars)
--       range ? match.refine(score, range) : nil
--     end.compact
--   end

--   def recompute_best_matches(all_matches)
--     return [] if all_matches.empty?

--     count = [@config.visible_choices, all_matches.count].min
--     matches = []

--     best_score = all_matches.min_by(&:score).score

--     # Consider matches, beginning with the best-scoring. A match always ranks
--     # higher than other matches with worse scores. However, the ranking between
--     # matches of the same score depends on other factors, so we always have to
--     # consider all matches of a given score.
--     (best_score..Float::INFINITY).each do |score|
--       matches += all_matches.select { |match| match.score == score }
--       # Stop if we have enough matches.
--       return sub_sort_matches(matches)[0, count] if matches.length >= count
--     end
--   end

--   def sub_sort_matches(matches)
--     matches.sort_by do |match|
--       [match.score, match.matching_range.count, match.choice.length]
--     end
--   end

--   def move_cursor(direction)
--     if max_visible_choices > 0
--       index = (@index + direction) % max_visible_choices
--       merge(:index => index)
--     else
--       self
--     end
--   end

--   class NoSelection; end
-- end
