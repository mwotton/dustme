{-# LANGUAGE DeriveGeneric #-}
module Dustme.Types where
import           Control.Concurrent.Async (Async)
import           Control.DeepSeq          (NFData)
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
import           GHC.Generics
import           System.Console.Terminfo
import           System.IO                (Handle)


data Match =
  Match
  { matchScore :: Int
  , matchStart :: Int
  , matchEnd   :: Int
  , matchText  :: Text
  } deriving (Show,Eq,Generic)

instance NFData Match

data Display a =
  Display { dispLinebreak  :: a
          , dispSelected   :: (Text -> a, Text -> a)
          , dispUnselected :: (Text -> a, Text -> a)
          }

newtype Search = Search Text
  deriving (Generic, Eq, Show)

instance Hashable Search

type SearchResult = [Match]

data SearchOp = AddText Text
              | Backspace
              | DeleteWord
  deriving (Eq,Show)

data Command
  = Accept
  | Edit SearchOp
  | Up
  | Down

type SearchCache = HashMap Search SearchResult
