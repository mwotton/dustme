{-# LANGUAGE OverloadedStrings #-}
module Selecta.TTY
  ( withTTY
  , TermOutput(..)
  , termText
  , getWidth
  , getHeight
  , termPrint
--  , highlight
  )
where
import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS8
import           Data.IORef
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Selecta.Types
import           System.Console.Terminfo
import           System.IO                        (BufferMode (NoBuffering),
                                                   Handle (..), IOMode (..),
                                                   hGetChar, hReady,
                                                   hSetBuffering, openFile)
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

withTTY :: FilePath -> (TTY -> IO ()) -> IO ()
withTTY fp f  = do
  t <- setupTermFromEnv
  h <- openFile "/dev/tty" ReadWriteMode
  hSetBuffering h NoBuffering
  p <- mkCommandReader h
  f (TTY h t p)


-- highlight tty = highlighter Red
--   where (Just highlighter) = getCapability (ttyTerm tty) withForegroundColor

mkCommandReader h = parser <$> newIORef ""
  where parser ref = do
          leftovers <- atomicModifyIORef ref (\x -> (x,""))
          res <- parseWith (getFromHandle h) commParser leftovers
          case res of
             Fail s a b -> error (show ("can't happen error", s, a, b))
             Partial _ -> error (show ("shouldn't happen - parseWith can resupply"))
             Done bs a -> do
               writeIORef ref bs
               return a

listify a = [a]

commParser = choice
--  _upParser
--  <|> _downParser
--  <|>
  [(Edit . const Backspace <$> string "\DEL")
  ,(Edit . const DeleteWord <$> string "\ETB")
  ,const Up <$> string "\SO"
  ,const Down <$> string "\DLE"
  ,const Accept <$> string "\n"
  ,(Edit . AddText . T.pack . listify <$> anyChar)
  ]
-- parseInput :: Char -> Maybe Command
-- parseInput t = case t of
--   '\ESC[B' -> Just Up
--   'n' -> Just Down
--   'p' -> Just Up
--   'b' -> Just (Edit Backspace)
--   '\n'  -> Just Accept
--   x -> Just (Edit $ AddText $ T.pack [x])

-- KEY_CTRL_C = ?\C-c
-- KEY_CTRL_N = ?\C-n
-- KEY_CTRL_P = ?\C-p
-- KEY_CTRL_U = ?\C-u
-- KEY_CTRL_H = ?\C-h
-- KEY_CTRL_W = ?\C-w
-- KEY_CTRL_J = ?\C-j
-- KEY_CTRL_M = ?\C-m
-- KEY_DELETE = 127.chr # Equivalent to ?\C-?

-- getSelection :: TTY -> IO Command
-- getSelection tty = parseWith (getAvailableData tty) (getStored
--   maybe (getSelection tty) return . parseInput
--     =<< getAvailableData tty

-- class TTY < Struct.new(:console_file)
--   def self.with_tty(&block)
--     # Selecta reads data from stdin and writes it to stdout, so we can't draw
--     # UI and receive keystrokes through them. Fortunately, all modern
--     # Unix-likes provide /dev/tty, which IO.console gives us.
--     console_file = IO.console
--     tty = TTY.new(console_file)
--     block.call(tty)
--   end


termPrint (TTY handle term _ ) = PP.hPutDoc handle

dowhile :: IO Bool -> IO a -> IO [a]
dowhile p f = (:) <$> f <*> while p f

while :: IO Bool -> IO a -> IO [a]
while p f = do
  go <- p
  if go
    then (:) <$> f <*> while p f
    else return []

getFromHandle :: Handle -> IO BS8.ByteString
getFromHandle h =
  BS8.pack <$> dowhile (hReady h) (hGetChar h)
--     input = console_file.getc
--     while console_file.ready?
--       input += console_file.getc
--     end
--     input

getWidth :: TTY -> Int
getWidth t = fromMaybe (error "width not defined") (getCapability (ttyTerm t) termColumns)

getHeight :: TTY -> Int
getHeight t = fromMaybe (error "heighnot defined") (getCapability (ttyTerm t) termLines)


--   def puts
--     console_file.puts
--   end



-- winsize :: TTY -> IO Int
-- winsize = undefined
--     console_file.winsize


-- shouldn't need these
--   def stty(args)
--     command("stty #{args}").strip
--   end

--   private

--   # Run a command with the TTY as stdin, capturing the output via a pipe
--   def command(command)
--     IO.pipe do |read_io, write_io|
--       pid = Process.spawn(command, :in => "/dev/tty", :out => write_io)
--       Process.wait(pid)
--       raise "Command failed: #{command.inspect}" unless $?.success?
--       write_io.close
--       read_io.read
--     end
--   end
-- end
