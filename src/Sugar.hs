module Sugar where

import           TypeChecker
import           TypeEnv
import           Operation
import           Types
import           Utils.Symbol
import           Utils.Error
import           AST
import           Eval
import           Utils.Substitution            as Sub
import           EffectRow
import           SugarTypes

import           Control.Monad
import           Data.Maybe                     ( fromMaybe )

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

-- Syntactic sugar for multiple declarations with type annotations. 
-- A file with the format:
-- id1 : tau1
-- id1 = def1;;
--
-- id2 : tau2
-- id2 = def2;;
-- ... 
--
-- main : tauMain
-- main = defMain
--
-- gets converted to a list of (Var, Type, Term). 
-- An id only is available to the definitions after it.
-- Every id (except for main) must be an expression.
-- After type-checking, the list gets converted to a nested let:
-- let id1 =  val (def1 : tau1) in let id2 = ... in defMain.


-- | Default effect environment.
sigma :: Sig
sigma =
  (th, TOp TNat TUnit) : (pr, TOp TNat TUnit) : (re, TOp TUnit TNat) : map
    (\i -> (opi i, TOp TUnit TUnit))
    [1 .. 4]
 where
  pr = toSymbol "Print"
  re = toSymbol "Read"
  th = toSymbol "Throw"
  opi i = toSymbol $ "Op" ++ show i

-- | Check if a list of declarations is valid. 
validDecs :: [Dec] -> Either Error ()
validDecs decs = do
  notEmpty decs
  allExpressions decs
  endsWithMain decs
  mainIsComp decs
 where
  notEmpty decs = when (null decs) $ Left "Empty program."
  allExpressions decs = unless (all (\(_, t, _) -> isValue t) (init decs))
    $ Left "All declarations (excepting main) should be value expressions."
  endsWithMain decs =
    unless ((\(n, _, _) -> n == toSymbol "main") (last decs))
      $ Left "Declarations must end with main."
  mainIsComp decs = unless ((\(_, t, _) -> isComp t) (last decs))
    $ Left "Main must be a computation."


renameAnnosE :: (TypeChecker m) => Exp -> m Exp
renameAnnosE (ESucc e       ) = renameAnnosE e >>= \e' -> return (ESucc e')
renameAnnosE (EFunc x c     ) = renameAnnosC c >>= \c' -> return (EFunc x c')
renameAnnosE (EHand x cv cls) = do
  cv'  <- renameAnnosC cv
  cls' <- mapM
    (\(op, x, k, ci) -> renameAnnosC ci >>= \ci' -> return (op, x, k, ci'))
    cls
  return (EHand x cv' cls')
renameAnnosE (EAnno e t) = do
  (t', _) <- rename (VT t)
  e'      <- renameAnnosE e
  return (EAnno e' (toVT t'))
renameAnnosE e = return e

renameAnnosC :: (TypeChecker m) => Comp -> m Comp
renameAnnosC (CVal e      ) = renameAnnosE e >>= \e' -> return (CVal e')
renameAnnosC (COp op e y c) = do
  e' <- renameAnnosE e
  c' <- renameAnnosC c
  return (COp op e' y c')
renameAnnosC (CWith e c) = do
  e' <- renameAnnosE e
  c' <- renameAnnosC c
  return (CWith e' c')
renameAnnosC (CApp e1 e2) = do
  e1' <- renameAnnosE e1
  e2' <- renameAnnosE e2
  return (CApp e1' e2')
renameAnnosC (CIf e c1 c2) = do
  e'  <- renameAnnosE e
  c1' <- renameAnnosC c1
  c2' <- renameAnnosC c2
  return (CIf e' c1' c2')
renameAnnosC (CMatch e c1 x c2) = do
  e'  <- renameAnnosE e
  c1' <- renameAnnosC c1
  c2' <- renameAnnosC c2
  return (CMatch e' c1' x c2')
renameAnnosC (CLet x c1 c2) = do
  c1' <- renameAnnosC c1
  c2' <- renameAnnosC c2
  return (CLet x c1' c2')
renameAnnosC (CAnno c t) = do
  (t', _) <- rename (CT t)
  c'      <- renameAnnosC c
  return (CAnno c' (toCT t'))

renameAnnos :: (TypeChecker m) => Term -> m Term
renameAnnos (E e) = renameAnnosE e >>= \e' -> return (E e')
renameAnnos (C c) = renameAnnosC c >>= \c' -> return (C c')

renameDecTypes :: (TypeChecker m) => [Dec] -> m [Dec]
renameDecTypes [(idMain, tMain, main)] = do
  main' <- renameAnnos main
  return [(idMain, tMain, main')]
renameDecTypes ((id, tau, def) : rest) = do
  (tau', _) <- rename tau
  def'      <- renameAnnos def
  rest'     <- renameDecTypes rest
  return ((id, tau', def') : rest')

decsChecker :: (TypeChecker m, TypeEnv e) => e -> [Dec] -> m ()
decsChecker _   []                      = return ()
decsChecker env ((id, tau, def) : rest) = do
  s <- checkType env def tau
  decsChecker (extEnv id (apply s tau) env) rest

-- | Desugar a list of declarations into a nested let, as explained before. 
-- Declarations are assumed valid.
desugarDecs :: [Dec] -> (Type, Term)
desugarDecs decs = (getMainType decs, desugarDecs' decs)
 where
  getMainType decs = (\(_, tmain, _) -> tmain) (last decs)
  toValT (VT t) = t
  toExp (E e) = e
  toComp (C c) = c
  desugarDecs' :: [Dec] -> Term
  desugarDecs' [(_, _, main)         ] = main
  desugarDecs' ((id, tau, def) : decs) = C $ CLet
    id
    (CVal (EAnno (toExp def) (toValT tau)))
    (toComp $ desugarDecs' decs)

-- | Type-check declarations, expanding the environment. 
checkDecs
  :: (Maybe Sig, [Dec])
  -> Either Error (Type, Term, Substitution EffVar EffRow, Sig)
checkDecs (sigM, decs) = validDecs decs >> runTC (renameAndCheck decs)
 where
  renameAndCheck decs = do
    decs' <- renameDecTypes decs
    let sig = sigma ++ fromMaybe [] sigM
    decsChecker (initEnv sig) decs'
    -- Sanity check: type still checks after desugaring.
    let (dsTau, dsTerm) = desugarDecs decs'
    s <- checkType (initEnv sig) dsTerm dsTau
    return (dsTau, dsTerm, s, sig)

-- | Run a computation term with initial env.
run :: Term -> Type -> Sig -> Either Error Comp
run (C c) (CT ct) sig = runSteps (initEnv sig) (c, Just ct)
