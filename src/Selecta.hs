{-# LANGUAGE OverloadedStrings #-}
module Selecta where

import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Debug.Trace      (trace)
import           Selecta.Renderer
import           Selecta.Score
import           Selecta.Search
import           Selecta.Text
import           Selecta.TTY      (withTTY)
import           Selecta.Types
import           System.IO

selecta config = do
  hSetBuffering stdout NoBuffering
  input <- T.lines <$> TIO.getContents
  print input
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
--      print ("rendering", index, matches, search)git im
      renderSearch tty index matches
      s <- ttyGetCommand tty
      case s of
        Accept  -> print "accept" >> TIO.putStrLn (matchText $ matches !! index)
        Up      -> print "up" >> go (clamp (length matches) (index-1)) matches search cache
        Down    -> print "down" >> go (clamp (length matches) (index+1)) matches search cache
        Edit op ->
          let (newsearch,newcandidates) = applyOp'  op  search matches
              (searchResult, newcache) =
--                trace (show ("edit2", newcandidates, newsearch))
                getResults newcandidates cache newsearch
          in go 0 searchResult newsearch newcache


clamp hi i
 | i < 0   = 0
 | i >= hi = hi - 1
 | otherwise       = i




--   def run_in_screen(config, screen, tty)
--     search = Search.from_config(config)
--     search = ui_event_loop(search, screen, tty)
--     search
--   end

--   # Use the search and screen to process user actions until they quit.
--   def ui_event_loop(search, screen, tty)
--     while not search.done?
--       Renderer.render!(search, screen)
--       search = handle_keys(search, tty)
--     end
--     search
--   end

--   def handle_keys(search, tty)
--     new_query_chars = ""

--     # Read through all of the buffered input characters. Process control
--     # characters immediately. Save any query characters to be processed
--     # together at the end, since there's no reason to process intermediate
--     # results when there are more characters already buffered.
--     tty.get_available_input.chars.each do |char|
--       is_query_char = !!(char =~ /[[:print:]]/)
--       if is_query_char
--         new_query_chars << char
--       else
--         search = handle_control_character(search, char)
--       end
--     end

--     if new_query_chars.empty?
--       search
--     else
--       search.append_search_string(new_query_chars)
--     end
--   end

--   # On each keystroke, generate a new search object
--   def handle_control_character(search, key)
--     case key

--     when KEY_CTRL_N then search.down
--     when KEY_CTRL_P then search.up

--     when KEY_CTRL_U then search.clear_query
--     when KEY_CTRL_W then search.delete_word
--     when KEY_CTRL_H, KEY_DELETE then search.backspace

--     when ?\r, KEY_CTRL_J, KEY_CTRL_M then search.done

--     when KEY_CTRL_C then raise Abort

--     else search
--     end
--   end

--   class Abort < RuntimeError; end
-- end

-- class Configuration < Struct.new(:height, :initial_search, :choices)
--   def initialize(height, initialize, choices)
--     # Constructor is defined to force argument presence; otherwise Struct
--     # defaults missing arguments to nil
--     super
--   end

--   def visible_choices
--     height - 1
--   end

--   def self.from_inputs(choices, options, screen_height=21)
--     # Shrink the number of visible choices if the screen is too small
--     if options.fetch(:height) == "full"
--       height = screen_height
--     else
--       height = [options.fetch(:height), screen_height].min
--     end

--     choices = massage_choices(choices)
--     Configuration.new(height, options.fetch(:search), choices)
--   end

--   def self.default_options
--     parse_options([])
--   end

--   def self.parse_options(argv)
--     options = {:search => "", :height => 21}

--     parser = OptionParser.new do |opts|
--       opts.banner = "Usage: #{$PROGRAM_NAME} [options]"

--       opts.on_tail("-h", "--help", "Show this message") do |v|
--         puts opts
--         exit
--       end

--       opts.on_tail("--version", "Show version") do
--         puts Selecta::VERSION.join('.')
--         exit
--       end

--       opts.on("--height lines", "Specify UI height in lines (including prompt).", "(Use `--height full` for full-screen)") do |height|
--         if height == "full"
--           # "full" is a valid height
--         elsif height.to_i < 2
--           raise OptionParser::InvalidOption.new(%{must be at least 2})
--         else
--           height = height.to_i
--         end
--         options[:height] = height
--       end

--       opts.on("-s", "--search SEARCH", "Specify an initial search string") do |search|
--         options[:search] = search
--       end
--     end

--     begin
--       parser.parse!(argv)
--     rescue OptionParser::InvalidOption => e
--       $stderr.puts e
--       $stderr.puts parser
--       exit 1
--     end

--     options
--   end

--   def self.massage_choices(choices)
--     choices.map do |choice|
--       # Encoding to UTF-8 with `:invalid => :replace` isn't good enough; it
--       # still leaves some invalid characters. For example, this string will fail:
--       #
--       # echo "девуш\xD0:" | selecta
--       #
--       # Round-tripping through UTF-16, with `:invalid => :replace` as well,
--       # fixes this. I don't understand why. I found it via:
--       #
--       # http://stackoverflow.com/questions/2982677/ruby-1-9-invalid-byte-sequence-in-utf-8
--       if choice.valid_encoding?
--         choice
--       else
--         utf16 = choice.encode('UTF-16', 'UTF-8', :invalid => :replace, :replace => '')
--         utf16.encode('UTF-8', 'UTF-16')
--       end.strip
--     end
--   end
-- end





-- class Screen
--   def self.with_screen
--     TTY.with_tty do |tty|
--       screen = self.new(tty)
--       screen.configure_tty
--       begin
--         raise NotATTY if screen.height == 0
--         yield screen, tty
--       ensure
--         screen.restore_tty
--         tty.puts
--       end
--     end
--   end

--   class NotATTY < RuntimeError; end

--   attr_reader :tty

--   def initialize(tty)
--     @tty = tty
--     @original_stty_state = tty.stty("-g")
--   end

--   def configure_tty
--     # -echo: terminal doesn't echo typed characters back to the terminal
--     # -icanon: terminal doesn't  interpret special characters (like backspace)
--     tty.stty("raw -echo -icanon")
--   end

--   def restore_tty
--     tty.stty("#{@original_stty_state}")
--   end

--   def suspend
--     restore_tty
--     begin
--       yield
--       configure_tty
--     rescue
--       restore_tty
--     end
--   end

--   def with_cursor_hidden(&block)
--     write_bytes(ANSI.hide_cursor)
--     begin
--       block.call
--     ensure
--       write_bytes(ANSI.show_cursor)
--     end
--   end

--   def height
--     tty.winsize[0]
--   end

--   def width
--     tty.winsize[1]
--   end

--   def cursor_up(lines)
--     write_bytes(ANSI.cursor_up(lines))
--   end

--   def newline
--     write_bytes("\n")
--   end

--   def write(text)
--     write_bytes(ANSI.clear_line)
--     write_bytes("\r")

--     text.components.each do |component|
--       if component.is_a? String
--         write_bytes(expand_tabs(component))
--       elsif component == :inverse
--         write_bytes(ANSI.inverse)
--       elsif component == :reset
--         write_bytes(ANSI.reset)
--       else
--         if component =~ /_/
--           fg, bg = component.to_s.split(/_/).map(&:to_sym)
--         else
--           fg, bg = component, :default
--         end
--         write_bytes(ANSI.color(fg, bg))
--       end
--     end
--   end

--   def expand_tabs(string)
--     # Modified from http://markmail.org/message/avdjw34ahxi447qk
--     tab_width = 8
--     string.gsub(/([^\t\n]*)\t/) do
--       $1 + " " * (tab_width - ($1.size % tab_width))
--     end
--   end

--   def write_bytes(bytes)
--     tty.console_file.write(bytes)
--   end
-- end


-- class ANSI
--   ESC = 27.chr

--   class << self
--     def escape(sequence)
--       ESC + "[" + sequence
--     end

--     def clear
--       escape "2J"
--     end

--     def hide_cursor
--       escape "?25l"
--     end

--     def show_cursor
--       escape "?25h"
--     end

--     def cursor_up(lines)
--       escape "#{lines}A"
--     end

--     def clear_line
--       escape "2K"
--     end

--     def inverse
--       escape("7m")
--     end

--     def reset
--       escape("0m")
--     end
--   end
-- end

-- getConsole
