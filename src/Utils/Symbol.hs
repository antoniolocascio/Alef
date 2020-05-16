module Utils.Symbol
  ( Symbol
  , toSymbol
  , fromSymbol
  , append
  , EffVar
  )
where

import qualified Data.Text                     as Text

-- | Symbol is a type synonym of Data.Text.Text 
type Symbol = Text.Text

toSymbol :: String -> Symbol
toSymbol = Text.pack

fromSymbol :: Symbol -> String
fromSymbol = Text.unpack

append :: Symbol -> Symbol -> Symbol
append = Text.append

-- Some useful type synonyms.
-- | An effect variable, EVar, is a symbol. 
type EffVar = Symbol
