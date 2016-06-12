{-# LANGUAGE DeriveGeneric #-}
module Selecta.Types where
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import           Data.Text               (Text)
import           GHC.Generics
import           System.Console.Terminfo
import           System.IO               (Handle)

data  TTY = TTY
  { ttyHandle     :: Handle
  , ttyTerm       :: Terminal
  , ttyGetCommand :: IO Command
  }

-- data Colour
--   = Black
--   | Red
--   | Green
--   | Yellow
--   | Blue
--   | Magenta
--   | Cyan
--   | White
--   | Default -- ???
--   deriving (Enum, Bounded, Eq,Ord)

-- fgColour :: Colour -> Int
-- fgColour Default = 39
-- fgColour x = 30 + fromEnum x

-- bgColour :: Colour -> Int
-- bgColour Default = 49
-- bgColour x = 40 + fromEnum x


data Match =
  Match
  { matchScore :: Int
  , matchStart :: Int
  , matchEnd   :: Int
  , matchText  :: Text
  } deriving (Show,Eq)


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
-- type SavedSearches = ([Text], HashMap Search SearchResult)
