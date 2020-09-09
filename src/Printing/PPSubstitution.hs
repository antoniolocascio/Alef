module Printing.PPSubstitution where

import           Prelude                 hiding ( (<>) )
import           Text.PrettyPrint

import           Utils.Symbol
import           Utils.Substitution

import           Printing.PPTypes

import           EffectRow

pSubstitution :: Substitution EffVar EffRow -> Doc
pSubstitution sub = brackets $ vcat
  (   punctuate (comma <> space)
  $   (\(mu, er) -> pEffVar mu <+> funcArrow <+> pEffRow er)
  <$> list sub
  )

renderSubstitution :: Substitution EffVar EffRow -> String
renderSubstitution = render . pSubstitution
