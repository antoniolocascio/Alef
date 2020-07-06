module Utils.Symbol
  ( Symbol
  , toSymbol
  , fromSymbol
  , append
  , EffVar
  , Var
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
-- | An effect variable is a symbol. 
type EffVar = Symbol

-- | We represent variable names with symbols.
type Var = Symbol
