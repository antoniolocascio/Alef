module Printing.PPSubstitution where

import           Prelude                 hiding ( (<>) )
import           Utils.Symbol
import           Utils.Substitution
import           EffectRow
import           Text.PrettyPrint
import           Printing.PPTypes

pSubstitution :: Substitution EffVar EffRow -> Doc
pSubstitution sub = brackets $ vcat
  (   punctuate (comma <> space)
  $   (\(mu, er) -> pEffVar mu <+> funcArrow <+> pEffRow er)
  <$> list sub
  )

renderSubstitution :: Substitution EffVar EffRow -> String
renderSubstitution = render . pSubstitution
