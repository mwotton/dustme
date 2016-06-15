{-# LANGUAGE OverloadedStrings #-}
module Dustme.Renderer where
import           Data.Char    (chr)
import           Data.List    (intersperse)
import           Data.Monoid  ((<>))
import qualified Data.Text    as T
import           Dustme.TTY
import           Dustme.Types
-- import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))


-- buildMatch :: Int -> Int -> Match -> Int -> Doc
buildMatches :: Monoid a
             => Display a
             -> Int
             -> Int
             -> Int
             -> [Match]
             -> Search
             -> (a, T.Text)
buildMatches display width height selected matches (Search search) =
  (body, searchLine)
  where
    searchLine = (T.pack . show)  (length matches) <> " > " <> search
    body = dispLinebreak display <>
           (mconcat . intersperse (dispLinebreak display)
            $ zipWith buildMatch matches [0..height - 2])

    buildMatch match current = normal before <> highlight matching <> normal end
        where
          (normal,highlight) =
            if  selected == current
            then dispSelected display -- (onwhite,onred . black)
            else dispUnselected display -- (id,red)

          fulltext = matchText match
          before = T.take (matchStart match) fulltext
          matching = T.drop (matchStart match) (T.take (matchEnd match + 1) fulltext)
          end = T.drop (matchEnd match + 1) fulltext


-- buildDoc width height index matches (Search search) = do

-- -- renderSearch :: TTY -> Int -> [Match] -> IO ()
-- -- renderSearch tty index matches (Search search) = do
--   let body =

--       searchLine =  ttext $
--   in (body,searchLine)

--   let width = getWidth tty
--       height = getHeight tty
--       body :: Doc
--       body =
--       searchLine :: T.Text

--       h = ttyHandle tty
