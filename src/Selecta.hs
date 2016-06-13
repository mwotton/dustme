{-# LANGUAGE OverloadedStrings #-}
module Selecta where

import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Debug.Trace      (trace)
import           Selecta.Renderer
import           Selecta.Search
import           Selecta.TTY      (withTTY)
import           Selecta.Types
import           System.IO

selecta config = do
  hSetBuffering stdout NoBuffering
  input <- T.lines <$> TIO.getContents
  let applyOp' = applyOp input
  withTTY "/dev/tty0" (setup applyOp' (map mkTrivial input))

mkTrivial = Match 10000 1 0

setup applyOp' initMatch tty = go 0 initMatch (Search "") emptyCache
  where
    go :: Int -> SearchResult -> Search -> SearchCache -> IO ()
    go    index  matches'         search    cache = do
      let matches = case search of
            (Search "") -> initMatch
            _ -> matches'
      renderSearch tty index matches search
      s <- ttyGetCommand tty
      case s of
        Accept  -> TIO.putStrLn (matchText $ matches !! index)
        Up      -> go (clamp (length matches) (index-1)) matches search cache
        Down    -> go (clamp (length matches) (index+1)) matches search cache
        Edit op ->
          let (newsearch,newcandidates) = applyOp'  op  search matches
              (searchResult, newcache) =
                getResults newcandidates cache newsearch
          in go 0 searchResult newsearch newcache

clamp hi i
 | i < 0   = 0
 | i >= hi = hi - 1
 | otherwise       = i
