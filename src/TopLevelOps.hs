module TopLevelOps where

import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import           Text.Parsec
import           System.IO

import           Utils.Symbol
import           Utils.Substitution
import           Utils.Error

import           Parser.Parser

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

import           TypeEnv
import           Operation
import           Types
import           AST
import           Eval
import           EffectRow
import           SugarTypes
import           Sugar

tlOps :: [Operation]
tlOps = [toSymbol "Print", toSymbol "Read", toSymbol "Throw"]

runTL :: Type -> Term -> Sig -> IO ()
runTL tau term sig = case run term tau sig of
  Left  e                  -> putStrLn ("Runtime error: \n" ++ e)
  Right opc@(COp op e y k) -> if op `elem` tlOps
    then runTLOp tau opc sig
    else putStrLn $ "--> " ++ renderTerm (C opc)
  Right c -> putStrLn $ "--> " ++ renderTerm (C c)

runTLOp :: Type -> Comp -> Sig -> IO ()
runTLOp tau (COp op e y k) sig
  | op == toSymbol "Print"
  = do
    putStrLn $ "> " ++ renderTerm (E e)
    runTL tau (apply (insert y EUnit empty) (C k)) sig
  | op == toSymbol "Read"
  = do
    putStr "> "
    hFlush stdout
    l <- getLine
    case parse natE "" l of
      Left  _ -> putStrLn "Runtime error: expected to read natural number."
      Right n -> runTL tau (apply (insert y n empty) (C k)) sig
  | op == toSymbol "Throw"
  = putStrLn $ "Runtime error: Throw called with argument: " ++ renderTerm (E e)
