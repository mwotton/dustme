module Main where

import           Selecta

main :: IO ()
main = print "calling" >> selecta 1

-- class Selecta
--   VERSION = [0, 0, 6]

--   def main
--     # We have to parse options before setting up the screen or trying to read
--     # the input in case the user did '-h', an invalid option, etc. and we need
--     # to terminate.
--     options = Configuration.parse_options(ARGV)
--     input_lines = $stdin.readlines

--     search = Screen.with_screen do |screen, tty|
--       begin
--         config = Configuration.from_inputs(input_lines, options, screen.height)
--         run_in_screen(config, screen, tty)
--       ensure
--         config.visible_choices.times { screen.newline }
--       end
--     end

--     unless search.selection == Search::NoSelection
--       puts search.selection
--     end
--   rescue Screen::NotATTY
--     $stderr.puts(
--       "Can't get a working TTY. Selecta requires an ANSI-compatible terminal.")
--     exit(1)
--   rescue Abort
--     # We were aborted via ^C.
--     #
--     # If we didn't mess with the TTY configuration at all, then ^C would send
--     # SIGINT to the entire process group. That would terminate both Selecta and
--     # anything piped into or out of it. Because Selecta puts the terminal in
--     # raw mode, that doesn't happen; instead, we detect the ^C as normal input
--     # and raise Abort, which leads here.
--     #
--     # To make pipelines involving Selecta behave as people expect, we send
--     # SIGINT to our own process group, which should exactly match what termios
--     # would do to us if the terminal weren't in raw mode. "Should!" <- Remove
--     # those scare quotes if ten years pass without this breaking!
--     #
--     # The SIGINT will cause Ruby to raise Interrupt, so we also have to handle
--     # that here.
--     begin
--       Process.kill("INT", -Process.getpgrp)
--     rescue Interrupt
--       exit(1)
--     end
--   end
