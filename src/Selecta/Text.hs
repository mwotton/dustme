module Selecta.Text where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           Selecta.Types




-- class Text
--   attr_reader :components

--   def self.[](*args)
--     if args.length == 1 && args[0].is_a?(Text)
--       # When called as `Text[some_text]`, just return the existing text.
--       args[0]
--     else
--       new(args)
--     end
--   end

--   def initialize(components)
--     @components = components
--   end

--   def ==(other)
--     components == other.components
--   end

--   def +(other)
--     Text[*(components + other.components)]
--   end

--   def truncate_to_width(width)
--     chars_remaining = width

--     # Truncate each string to the number of characters left within our
--     # allowed width. Leave anything else alone. This may result in empty
--     # strings and unused ANSI control codes in the output, but that's fine.
--     components = self.components.map do |component|
--       if component.is_a?(String)
--         component = component[0, chars_remaining]
--         chars_remaining -= component.length
--       end
--       component
--     end

--     Text.new(components)
--   end
-- end
