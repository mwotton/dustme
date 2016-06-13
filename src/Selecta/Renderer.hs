{-# LANGUAGE OverloadedStrings #-}
module Selecta.Renderer where
import           Data.Char                    (chr)
import           Data.List                    (intersperse)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import           Selecta.TTY
import           Selecta.Types

import           System.Console.ANSI          (hClearFromCursorToScreenEnd,
                                               hClearLine, hClearScreen,
                                               hCursorDown, hCursorUp,
                                               hSetCursorColumn, hShowCursor)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

buildMatch :: Int -> Int -> Match -> Int -> Doc
buildMatch width selected match current =
  let (normal,highlight) =
        if  selected == current
        then (onwhite,onred . black)
        else (id,red)
      fulltext = matchText match
      before = ttext $ T.take (matchStart match) fulltext
      matching = ttext $ T.drop (matchStart match) (T.take (matchEnd match + 1) fulltext)
      end = ttext $ T.drop (matchEnd match + 1) fulltext
      ttext = text . T.unpack
  in normal before <> highlight matching <> normal end

tshow :: Show a => a -> T.Text
tshow = T.pack . show

ttext :: T.Text -> Doc
ttext = text . T.unpack

-- renderSearch :: TTY -> Int -> [Match] -> IO ()
renderSearch tty index matches (Search search) = do
  let width = getWidth tty
      body :: Doc
      body = linebreak <> (mconcat . intersperse linebreak
             $ zipWith (buildMatch width index) matches [0..])
      searchLine :: T.Text
      searchLine =  tshow (length matches) <> " > " <> search
      h = ttyHandle tty
  hClearFromCursorToScreenEnd h
  termPrint tty body
  hCursorUp h (length matches)
  hSetCursorColumn h 0
  hClearLine h
  termPrint tty (ttext searchLine)
  hShowCursor h

termPrint (TTY handle term _ ) = hPutDoc handle
