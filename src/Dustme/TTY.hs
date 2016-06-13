{-# LANGUAGE OverloadedStrings #-}
module Dustme.TTY
  ( withTTY
  , TermOutput(..)
  , getWidth
  , getHeight
  )
where
import           Control.Applicative              ((<|>))
import           Control.Concurrent               (newEmptyMVar, putMVar)
import           Control.Concurrent.Async         (async, cancel)
import           Control.Exception                (bracket)
import           Control.Monad                    (forever)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS8
import           Data.IORef
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Dustme.Types
import           System.Console.Terminfo
import           System.IO                        (BufferMode (NoBuffering),
                                                   Handle (..), IOMode (..),
                                                   hGetChar, hReady,
                                                   hSetBuffering, openFile)
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

withTTY :: FilePath -> (TTY -> IO ()) -> IO ()
withTTY fp  = bracket setup teardown
  where setup = do
          t <- setupTermFromEnv
          h <- openFile "/dev/tty" ReadWriteMode
          hSetBuffering h NoBuffering
          mv <- newEmptyMVar
          p <- mkCommandReader h
          reader <- async $ forever  (p >>= putMVar mv)
          return (TTY h t mv reader)

        teardown (TTY h t mv reader) = cancel reader

mkCommandReader h = parser <$> newIORef ""
  where
    parser ref = do
      leftovers <- atomicModifyIORef ref (\x -> (x,""))
      res <- parseWith (getFromHandle h) commParser leftovers
      case res of
        Fail s a b -> error (show ("can't happen error", s, a, b))
        Partial _ -> error "shouldn't happen - parseWith can resupply"
        Done bs a -> do
          writeIORef ref bs
          return a

commParser = choice
  [ Edit . const Backspace <$> string "\DEL"
  , Edit . const DeleteWord <$> string "\ETB"
  , const Down <$> string "\SO"
  , const Up <$> string "\DLE"
  , const Accept <$> string "\n"
  , Edit . AddText . T.pack . listify <$> anyChar
  ]
  where listify a = [a]
-- still need to treat any of these specially?
-- KEY_CTRL_C = ?\C-c
-- KEY_CTRL_N = ?\C-n
-- KEY_CTRL_P = ?\C-p
-- KEY_CTRL_U = ?\C-u
-- KEY_CTRL_H = ?\C-h
-- KEY_CTRL_W = ?\C-w
-- KEY_CTRL_J = ?\C-j
-- KEY_CTRL_M = ?\C-m
-- KEY_DELETE = 127.chr # Equivalent to ?\C-?

dowhile :: IO Bool -> IO a -> IO [a]
dowhile p f = (:) <$> f <*> while p f

while :: IO Bool -> IO a -> IO [a]
while p f = do
  go <- p
  if go
    then (:) <$> f <*> while p f
    else return []

getFromHandle :: Handle -> IO BS8.ByteString
getFromHandle h = BS8.pack <$> dowhile (hReady h) (hGetChar h)

getWidth :: TTY -> Int
getWidth t = fromMaybe (error "width not defined") (getCapability (ttyTerm t) termColumns)

getHeight :: TTY -> Int
getHeight t = fromMaybe (error "heighnot defined") (getCapability (ttyTerm t) termLines)
