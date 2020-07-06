module Utils.Error where

import           Types
import           Printing.PPTypes
import           Printing.PPAST
import           Printing.PPSubstitution
import           Utils.Substitution
import           AST
import           EffectRow
import           Utils.Symbol
import           Data.Either                    ( either )

type Error = String

class Monad m => Fallible m where
  throw :: Error -> m a

typeError :: Type -> Type -> Term -> Error
typeError expT synthT term =
  "Type error:\n"
    ++ "\tExpected: "
    ++ renderType expT
    ++ "\n"
    ++ "\tSynthesized: "
    ++ renderType synthT
    ++ "\n"
    ++ "\tFor term:\n"
    ++ renderTerm term

expHasCompType :: Term -> Type -> Error
expHasCompType term synthT =
  "Expression should have a value type:"
    ++ "\n"
    ++ "Synthesized: "
    ++ renderType synthT
    ++ "\n"
    ++ "For term:\n"
    ++ renderTerm term

compHasValType :: Term -> Type -> Error
compHasValType term synthT =
  "Computation should have a computation type:"
    ++ "\n"
    ++ "Synthesized: "
    ++ renderType synthT
    ++ "\n"
    ++ "For term:\n"
    ++ renderTerm term

notAFunction :: Term -> Term -> Type -> Error
notAFunction term context synthT =
  "Expression not a function:"
    ++ "\n"
    ++ "Synthezised: "
    ++ renderType synthT
    ++ "\n"
    ++ "For term:\n"
    ++ renderTerm term
    ++ "\n"
    ++ "In context:\n"
    ++ renderTerm context

notAHandler :: Term -> Term -> Type -> Error
notAHandler term context synthT =
  "Expression not a handler:"
    ++ "\n"
    ++ "Synthezised: "
    ++ renderType synthT
    ++ "\n"
    ++ "For term:\n"
    ++ renderTerm term
    ++ "\n"
    ++ "In context:\n"
    ++ renderTerm context

cannotSynth :: Term -> Error
cannotSynth term =
  "Term's type cannot be synthesized:"
    ++ "\n"
    ++ "For term:\n"
    ++ renderTerm term

notAlphaEq :: Type -> Substitution EffVar EffRow -> Either Var Term -> Error
notAlphaEq t sub term =
  "Type cannot be checked, types not alpha-equivalent: "
    ++ "\n"
    ++ "Expected type: "
    ++ renderType t
    ++ "\n"
    -- ++ "With instantiation: \n"
    -- ++ renderSubstitution sub
    -- ++ "\n"
    ++ "Actual type: "
    ++ renderType (apply sub t)
    ++ "\n"
    ++ "For Term:\n"
    ++ renderTerm (either (E . EVar) id term)

diffEffVarsHand :: EffVar -> EffVar -> Term -> Error
diffEffVarsHand mu1 mu2 term =
  "Different effect varibles in handler type: "
    ++ fromSymbol mu1
    ++ " "
    ++ fromSymbol mu2
    ++ "\n"
    ++ "In handler:\n"
    ++ renderTerm term

opsNotCapturedHand :: Term -> Error
opsNotCapturedHand term =
  "Operations not handled by handler present in delta not present in delta':"
    ++ "\nIn handler:\n"
    ++ renderTerm term

incompatibleTypes :: Type -> Type -> Term -> Error
incompatibleTypes t1 t2 t =
  "Incompatible types: "
    ++ "\nType: "
    ++ renderType t1
    ++ "\nAnd type: "
    ++ renderType t2
    ++ "\nFor term:\n"
    ++ renderTerm t

nondeterministicHandler :: Error
nondeterministicHandler = "Non deterministic handler."
