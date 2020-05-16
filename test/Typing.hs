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
  testCheck e1 p1 t1
  testHeader 2
  testSynth e2 p2
  testHeader 3.1
  testCheck e3_1 p3_1 t3_1
  testHeader 3.2
  testSynth e3_2 p3_2
  testHeader 4
  testCheck e4 p4 t4
  testHeader 6
  testCheck e6 p6 t6
  testHeader 7
  testCheck e7 p7 t7


testCheck :: EnvC -> Term -> Type -> IO ()
testCheck env t tau = case runCheck env t tau of
  (Left  e) -> putStrLn e
  (Right s) -> do
    putStrLn "Success!"
    putStrLn "Term:"
    putStrLn $ renderTerm t
    putStrLn $ "Checked type: " ++ renderType tau
    putStrLn "Resulting substitution:"
    putStrLn $ renderSubstitution s
    putStrLn ""

testSynth :: EnvC -> Term -> IO ()
testSynth env t = case runSynth env t of
  (Left  e       ) -> putStrLn e
  (Right (tau, s)) -> do
    putStrLn "Success!"
    putStrLn "Term:"
    putStrLn $ renderTerm t
    putStrLn "Resulting type:"
    putStrLn $ renderType tau
    putStrLn ""


