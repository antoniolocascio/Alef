import           Operation
import           Types
import           AST
import           TypeChecker
import           Utils.Symbol
import           Utils.Set
import           Utils.Substitution             ( apply )
import           TypeEnv
import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST
import           Eval
import           Common
import           EffectRow
import           QCInstances
import           Test.QuickCheck
-- import           Test.QuickCheck.Property
import           Data.Either                    ( isRight
                                                , fromRight
                                                )

main :: IO ()
main = do
  putStrLn ""
  testHeader "Preservation"
  quickCheckWith (stdArgs { maxSize = 50, maxSuccess = 10000 })
                 prop_preservation
  testHeader "Progress"
  quickCheckWith (stdArgs { maxSize = 50, maxSuccess = 10000 }) prop_progress
  -- quickCheck (withMaxSuccess 100000 prop_synth_invariant)
  -- quickCheck (withMaxSuccess 100000 prop_synth_checks)

prop_preservation :: TypedTerm -> Property
prop_preservation (TT term typ) =
  counterexample (getError (takeStep term) typ)
    $   checksType term typ
    &&  canStep term
    ==> checksType (takeStep term) typ

prop_progress :: TypedTerm -> Property
prop_progress (TT term typ) =
  counterexample (getError term typ)
    $   checksType term typ
    ==> (canStep term && succesfulStep term)
    ||  isVal term
    ||  isOpInType term typ


checksType :: Term -> Type -> Bool
checksType term typ = isRight $ runCheck (initEnv sigma) term typ

isOpInType :: Term -> Type -> Bool
isOpInType (C (COp op _ _ _)) (CT (TComp _ er)) = hasOp op er
isOpInType _                  _                 = False

isVal :: Term -> Bool
isVal (C CVal{}) = True
isVal (E _     ) = True
isVal _          = False

canStep :: Term -> Bool
canStep (E _     ) = False
canStep (C COp{} ) = False
canStep (C CVal{}) = False
canStep _          = True

succesfulStep :: Term -> Bool
succesfulStep (C c) = isRight $ runStep (initEnv sigma) c

takeStep :: Term -> Term
takeStep (C c) = case runStep (initEnv sigma) c of
  Left  e  -> error e
  Right c' -> C c'

prop_checksType :: TypedTerm -> Property
prop_checksType (TT term typ) =
  counterexample (getError term typ) (checksType term typ)

getError :: Term -> Type -> String
getError term typ = case runCheck (initEnv sigma) term typ of
  (Left  e) -> "Error: " ++ e
  (Right s) -> "No error: " ++ renderSubstitution s

prop_synth_invariant :: TypedTerm -> Property
prop_synth_invariant (TT term tau') =
  checksType term tau' ==> case runSynth (initEnv sigma) term of
    (Left  e       ) -> False
    (Right (tau, s)) -> tau == apply s tau


prop_synth_checks :: TypedTerm -> Property
prop_synth_checks (TT term tau') =
  checksType term tau' ==> case runSynth (initEnv sigma) term of
    (Left  e       ) -> error e
    (Right (tau, s)) -> prop_checksType (TT term tau)
