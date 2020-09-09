module Printing.PPTypes where

import           Prelude                 hiding ( (<>) )
import           Text.PrettyPrint

import           Utils.Symbol
import qualified Utils.Set                     as S

import           Types
import           EffectRow
import           Operation

pipe :: Doc
pipe = text "|"

langle :: Doc
langle = text "<"

rangle :: Doc
rangle = text ">"

angles :: Doc -> Doc
angles d = langle <> d <> rangle

funcArrow :: Doc
funcArrow = text "->"

handArrow :: Doc
handArrow = text "->>"

pOperation :: Operation -> Doc
pOperation = text . fromSymbol

pDelta :: Delta -> Doc
pDelta d = hcat (punctuate (comma <> space) $ pOperation <$> S.toList d)

pEffVar :: EffVar -> Doc
pEffVar = text . fromSymbol

pEffRow :: EffRow -> Doc
pEffRow (delta, mu) | S.isEmpty delta = angles $ pEffVar mu
                    | otherwise = angles $ pDelta delta <+> pipe <+> pEffVar mu

pVType :: VType -> Doc
pVType TBool       = text "bool"
pVType TNat        = text "nat"
pVType TUnit       = text "unit"
pVType TEmpty      = text "empty"
pVType (TFunc a c) = parens $ pVType a <+> funcArrow <+> pCType c
pVType (THand c d) = parens $ pCType c <+> handArrow <+> pCType d

pCType :: CType -> Doc
pCType (TComp a e) = pVType a <> pEffRow e

pType :: Type -> Doc
pType (VT vt) = pVType vt
pType (CT ct) = pCType ct

renderType :: Type -> String
renderType = render . pType
