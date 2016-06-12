
module Selecta.Renderer where
import           Data.List                    (intersperse)
import qualified Data.Text                    as T
import           Selecta.TTY
import           Selecta.Types
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))




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


--     search_line = Text["#{match_count_label} > " + search.query]

--     matches = search.best_matches[0, search.config.visible_choices]
--     matches = matches.each_with_index.map do |match, index|
--       if index == search.index
--         Text[:inverse] + match.to_text + Text[:reset]
--       else
--         match.to_text
--       end
--     end
--     matches = correct_match_count(matches)
--     lines = [search_line] + matches
--     Rendered.new(lines, search_line)

--   def correct_match_count(matches)
--     limited = matches[0, search.config.visible_choices]
--     padded = limited + [Text[""]] * (search.config.visible_choices - limited.length)
--     padded
--   end

--   def match_count_label
--     choice_count = search.original_matches.length
--     max_label_width = choice_count.to_s.length
--     match_count = search.all_matches.count
--     match_count.to_s.rjust(max_label_width)
--   end

-- renderSearch :: TTY -> Int -> [Match] -> IO ()
renderSearch tty index matches =
  let width = getWidth tty
      text :: Doc
      text = mconcat . intersperse linebreak
             $ zipWith (buildMatch width index) matches [0..]
      --     screen.with_cursor_hidden do
    in termPrint tty text



-- render :: Match -> [(Color,Text)]
-- render m = mapMaybe (\(c,start,end) ->
--                        (c,) <$> sliceText (matchText m) start end)
--   [(Default, 0, beg - 1)
--   ,(Red,     beg, end)
--   ,(Default, end+1 , len)
--   ]
--   where beg = matchStart m
--         end = matchEnd m
--         len = T.length (matchText m)


-- sliceText :: Text -> Int -> Int -> Maybe Text
-- sliceText t from to = do
--   guard (from >=0)
--   guard (to <= len)
--   guard (from <= to)
--   return $ T.drop from $ T.dropEnd (len - to) t
--   where len = T.length t
