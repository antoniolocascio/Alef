import           Data.Either                    ( isRight )
import           Test.QuickCheck

import           Utils.Symbol
import           Utils.Set

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

import           Operation
import           Types
import           AST
import           TypeChecker
import           TypeEnv
import           Eval
import           Common
import           QCInstances

main :: IO ()
main =
  quickCheckWith (stdArgs { maxSize = 50, maxSuccess = 10000 }) prop_checksType

prop_checksType :: TypedTerm -> Property
prop_checksType (TT term typ) = counterexample
  (getError term typ)
  (isRight $ runCheck (initEnv sigma) term typ)

getError :: Term -> Type -> String
getError term typ = case runCheck (initEnv sigma) term typ of
  (Left  e) -> "Error: " ++ e
  (Right s) -> "No error: " ++ renderSubstitution s

