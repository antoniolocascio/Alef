module SugarTypes where

import           AST
import           Types
import           Operation
import           EffectRow

-- | Type synonym for a declaration.
type Dec = (Var, Type, Term)

-- | Type synonym for an effect signatue.
type Sig = [(Operation, OpType)]
