{-# LANGUAGE OverloadedStrings #-}
module Dustme.TTY
  ( withTTY
  , TermOutput(..)
--  , getWidth
--  , getHeight
  , renderSearch
  , TTY(..)
  )
where
import           Control.Applicative          ((<|>))
import           Control.Concurrent           (newEmptyMVar, putMVar)
import           Control.Concurrent.Async     (Async, async, cancel)
import           Control.Concurrent.MVar      (MVar)
import           Control.Exception            (bracket)
import           Control.Monad                (forever)
import qualified Data.ByteString.Char8        as BS8
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Dustme.Types
import           System.Console.ANSI          (hClearFromCursorToScreenEnd,
                                               hClearLine, hClearScreen,
                                               hCursorDown, hCursorUp,
                                               hSetCursorColumn, hShowCursor)
import           System.Console.Terminfo
import           System.IO                    (BufferMode (NoBuffering),
                                               Handle (..), IOMode (..),
                                               hGetChar, hReady, hSetBuffering,
                                               openFile)
import           Text.PrettyPrint.ANSI.Leijen (Doc, hPutDoc, text)

withTTY ::  FilePath
        -> (IO BS8.ByteString -> IO (IO b))
        -> (TTY b -> IO c) -> IO c
withTTY fp mkCommandReader  = bracket setup teardown
  where setup = do
          t <- setupTermFromEnv
          h <- openFile fp ReadWriteMode
          hSetBuffering h NoBuffering
          mv <- newEmptyMVar
          p <- mkCommandReader (getFromHandle h)
          reader <- async $ forever  (p >>= putMVar mv)
          return (TTY h t mv reader)

        teardown (TTY h t mv reader) = cancel reader


getFromHandle :: Handle -> IO BS8.ByteString
getFromHandle h = BS8.pack <$> dowhile (hReady h) (hGetChar h)

getWidth :: TTY c -> Int
getWidth t = fromMaybe (error "width not defined") (getCapability (ttyTerm t) termColumns)

getHeight :: TTY c -> Int
getHeight t = fromMaybe (error "heighnot defined") (getCapability (ttyTerm t) termLines)

dowhile :: IO Bool -> IO a -> IO [a]
dowhile p f = (:) <$> f <*> while p f

while :: IO Bool -> IO a -> IO [a]
while p f = do
  go <- p
  if go
    then (:) <$> f <*> while p f
    else return []

data  TTY c = TTY
  { ttyHandle     :: Handle
  , ttyTerm       :: Terminal
  , ttyGetCommand :: MVar c
  , ttyProcess    :: Async ()
  }

termPrint (TTY handle term _ _ ) = hPutDoc handle

renderSearch buildDoc tty index matches search = do
  let (body, searchLine) = buildDoc (getWidth tty) (getHeight tty) index matches search
      h = ttyHandle tty
  hClearFromCursorToScreenEnd h
  termPrint tty body
  hCursorUp h (length matches)
  hSetCursorColumn h 0
  hClearLine h
  termPrint tty (ttext searchLine)
  hShowCursor h


tshow :: Show a => a -> T.Text
tshow = T.pack . show

ttext :: T.Text -> Doc
ttext = text . T.unpack
