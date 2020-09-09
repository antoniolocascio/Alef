import           Data.Either                    ( isRight
                                                , fromRight
                                                )
import           Test.QuickCheck

import           Utils.Symbol
import           Utils.Set
import           Utils.Error

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
main = quickCheckWith (stdArgs { maxSize = 50, maxSuccess = 10000 }) prop_step


testStep :: Comp -> IO ()
testStep c = case runStep (initEnv sigma) c of
  (Left  e ) -> putStrLn e
  (Right c') -> do
    putStrLn "Success!"
    putStrLn "Computation:\n"
    putStrLn $ renderTerm (C c)
    putStrLn "\nEvaluates in one step to:\n"
    putStrLn $ renderTerm (C c')
    putStrLn ""


prop_step :: TypedTerm -> Property
prop_step (TT term typ) = canStep term ==> isRight $ takeStep term

canStep :: Term -> Bool
canStep (E _     ) = False
canStep (C COp{} ) = False
canStep (C CVal{}) = False
canStep _          = True


takeStep :: Term -> Either Error Comp
takeStep (C c) = runStep (initEnv sigma) c


getError :: Term -> Type -> String
getError term typ = case runCheck (initEnv sigma) term typ of
  (Left  e) -> "Error: " ++ e
  (Right s) -> "No error: " ++ renderSubstitution s

