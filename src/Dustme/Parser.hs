{-# LANGUAGE OverloadedStrings #-}
module Dustme.Parser where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.IORef
import qualified Data.Text                        as T
import           Dustme.Types
import           System.IO                        (BufferMode (NoBuffering),
                                                   Handle (..), IOMode (..),
                                                   hGetChar, hReady,
                                                   hSetBuffering, openFile)

mkCommandReader
  :: IO ByteString
  -> IO (IO Command)
mkCommandReader getMore = parser <$> newIORef ""
  where
    parser ref = do
      leftovers <- atomicModifyIORef ref (\x -> (x,""))
      res <- parseWith getMore commParser leftovers
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
