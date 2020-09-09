module Main where

import           Data.Either
import           Data.Maybe
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad

import           Utils.Substitution            as Sub
import           Utils.Symbol

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

import           Types
import           TypeChecker
import           Parser.Parser
import           Sugar
import           AST
import           Eval
import           EffectRow
import           SugarTypes
import           TopLevelOps

data Options = Options
  {
    -- | Only type-check, don't execute.
    optType :: Bool
    -- | Print help.
  , optHelp :: Bool
    -- | Set verbosity.
  , optVerb :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Options { optType = False, optHelp = False, optVerb = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['t']
           ["type"]
           (NoArg (\opts -> opts { optType = True }))
           "Only type-check, don't execute."
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "Print help."
  , Option ['v']
           ["verbosity"]
           (NoArg (\opts -> opts { optVerb = True }))
           "Error message verbosity."
  ]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage:"

-- OnlyType  Verbose  Term
evaluateIfNeeded
  :: Bool -> Bool -> (Type, Term, Substitution EffVar EffRow, Sig) -> IO ()
evaluateIfNeeded True vb (_, _, sub, _) = do
  putStrLn "Type-checks!"
  when
    vb
    (do
      putStrLn "Resulting substitution:"
      putStrLn $ renderSubstitution sub
    )
evaluateIfNeeded _ _ (tau, term, _, sig) = runTL tau term sig

main :: IO ()
main = do
  s : opts   <- Env.getArgs
  (opts', _) <- compilerOptions opts
  when (optHelp opts') (putStrLn (usageInfo "Usage: " options))
  sourceCode <- readFile s
  let eTypeTermSub = parseDecs sourceCode >>= checkDecs
  either (putStrLn . ("Error: \n" ++))
         (evaluateIfNeeded (optType opts') (optVerb opts'))
         eTypeTermSub



