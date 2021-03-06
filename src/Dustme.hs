{-# LANGUAGE OverloadedStrings #-}
module Dustme where

import           Control.Concurrent       (newEmptyMVar, takeMVar)
import           Control.Concurrent.Async (async, cancel, race)
import           Control.DeepSeq          (force)
import           Control.Exception        (bracket, evaluate)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Dustme.Renderer
import           Dustme.Search
import           Dustme.TTY               (withTTY)
import           Dustme.Types
import           System.IO

dustme config = do
  hSetBuffering stdout NoBuffering
  input <- T.lines <$> TIO.getContents
  let applyOp' = applyOp input
  withTTY "/dev/tty0" (setup applyOp' (map mkTrivialMatch input))

mkTrivialMatch = Match 10000 1 0

setup applyOp' initMatch tty = go 0 initMatch (Search "") emptyCache
  where
    getCommand = takeMVar (ttyGetCommand tty)

    go :: Int -> SearchResult -> Search -> SearchCache -> IO ()
    go    index  matches'         search    cache = do
      let matches = case search of
            (Search "") -> initMatch
            _ -> matches'

      -- the idea here is that that ttyGetCommand can always take
      -- priority over printing. This means that if we type a bunch
      -- of keys in in quick succession, the UI will remain responsive.
      r <- race (evaluate $ force matches) getCommand
      s <- case r of
             Left matches' -> renderSearch tty index matches search >> getCommand
             Right x -> return x

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
 | i < 0     = 0
 | i >= hi   = hi - 1
 | otherwise = i
