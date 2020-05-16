import           Operation
import           Types
import           AST
import           TypeChecker
import           Utils.Symbol
import           Utils.Set
import           TypeEnv
import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST
import           Common
import           Parser.Parser
import           QCInstances
import           Test.QuickCheck

main :: IO ()
main = quickCheckWith stdArgs{maxSize = 20}  $ withMaxSuccess 1000  prop_parses

instance Show Type where
  show = renderType

prop_parses :: TypedTerm -> Property
prop_parses (TT term _) = counterexample
  (getError term)
  (case parseTerm (renderTerm term) of
    Left  _     -> False
    Right term' -> term == term'
  )


getError :: Term -> String
getError term = case parseTerm (renderTerm term) of
  (Left e) -> "Error: " ++ show e
  (Right term') ->
    "Not equal: \n" ++ renderTerm term ++ "\n" ++ renderTerm term'
