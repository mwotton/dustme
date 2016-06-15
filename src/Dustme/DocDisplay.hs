module Dustme.DocDisplay where

import qualified Data.Text                    as T
import           Dustme.Types
import qualified Text.PrettyPrint.ANSI.Leijen as PP

docDisplay = Display
      { dispLinebreak = PP.linebreak
      , dispSelected = (PP.onwhite . ttext, PP.onred.PP.black . ttext)
      , dispUnselected = ( ttext, PP.red . ttext)
      }


ttext = PP.text . T.unpack
