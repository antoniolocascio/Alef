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

main :: IO ()
main = do
  testHeader 1
  printTermType p1 t1
  testHeader 2
  printTerm p2
  testHeader 3.1
  printTermType p3_1 t3_1
  testHeader 3.2
  printTerm p3_2
  testHeader 4
  printTermType p4 t4
  testHeader 5
  printTerm (C c5)
  testHeader 6
  printTermType p6 t6

printTerm :: Term -> IO ()
printTerm t = do
  putStrLn "Term:"
  putStrLn $ renderTerm t

printTermType :: Term -> Type -> IO ()
printTermType t tau = do
  putStrLn $ "Type: " ++ renderType tau
  putStrLn "Term:"
  putStrLn $ renderTerm t

