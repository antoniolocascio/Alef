module Printing.PPAST where

import           Prelude                 hiding ( (<>) )
import           Operation
import           Utils.Symbol
import qualified Utils.Set                     as S
import           Types
--import           EffectRow
import           AST
import           Printing.PPTypes
import           Text.PrettyPrint

tabW :: Int
tabW = 2

pVar :: Var -> Doc
pVar = text . fromSymbol

pExp :: Exp -> Doc
pExp (EVar v)    = pVar v
pExp ETrue       = text "true"
pExp EFalse      = text "false"
pExp EZero       = text "0"
pExp n@(ESucc e) = pNat n
pExp EUnit       = lparen <> rparen
pExp (EFunc x c) = parens $ text "fun" <+> pVar x <+> funcArrow <+> pComp c
pExp (EHand x cv clauses) =
  parens
    $   text "handler"
    <+> text "val"
    <+> pVar x
    <+> funcArrow
    <+> pComp cv
    <>  comma
    $$  braces (nest tabW (pClauses clauses))
pExp (EAnno e t) = parens $ pExp e <+> text ":" <+> pVType t

pNat :: Exp -> Doc
pNat e | isLit e   = int (toInt e)
       | otherwise = expand e
 where
  isLit EZero     = True
  isLit (ESucc e) = isLit e
  isLit e         = False
  toInt EZero     = 0
  toInt (ESucc e) = 1 + toInt e
  expand (ESucc e) = parens $ text "succ" <+> expand e
  expand e         = pExp e


pClauses :: [(Operation, Var, Var, Comp)] -> Doc
pClauses = vcat . punctuate comma . map
  (\(op, x, k, ci) ->
    pOperation op <+> pVar x <+> pVar k <+> funcArrow <+> pComp ci
  )

pComp :: Comp -> Doc
pComp (CVal e) = parens $ text "val" <+> pExp e
pComp (COp op e y k) =
  parens $ pOperation op <+> pExp e <+> parens (pVar y <> text "." <> pComp k)
pComp (CWith h c) =
  parens $ text "with" $$ nest tabW (pExp h) $$ text "handle" $$ nest
    tabW
    (pComp c)
pComp (CApp e1 e2) = parens $ pExp e1 <+> pExp e2
pComp (CIf e c1 c2) =
  parens $ text "if" <+> pExp e $$ nest tabW (text "then" <+> pComp c1) $$ nest
    tabW
    (text "else" <+> pComp c2)
pComp (CMatch e c1 x c2) =
  parens
    $   text "match"
    <+> pExp e
    <+> text "with"
    $$  nest tabW (text "| 0" <+> funcArrow <+> pComp c1)
    $$  nest tabW (text "| succ" <+> pVar x <+> funcArrow <+> pComp c2)
pComp (CLet x c1 c2) =
  parens
    $   text "let"
    <+> (pVar x <+> text "=" <+> pComp c1)
    <+> text "in"
    $$  pComp c2

pTerm :: Term -> Doc
pTerm (E e) = pExp e
pTerm (C c) = pComp c

renderTerm :: Term -> String
renderTerm = render . pTerm
