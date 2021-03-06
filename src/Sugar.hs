module Sugar where

import           Control.Monad
import           Data.Maybe                     ( fromMaybe )

import           Utils.Symbol
import           Utils.Error
import           Utils.Unique
import           Utils.Substitution            as Sub

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

import           TypeChecker
import           TypeEnv
import           Operation
import           Types
import           AST
import           Eval
import           EffectRow
import           SugarTypes

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
-- The list gets converted to a nested let:
-- let id1 =  val (def1 : tau1) in let id2 = ... in defMain.


-- | Default effect environment.
sigma :: Sig
sigma =
  (th, TOp TNat TEmpty) : (pr, TOp TNat TUnit) : (re, TOp TUnit TNat) : map
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

isUnderscore :: Symbol -> Bool
isUnderscore = (== toSymbol "_")

fillVar :: (UniqueGen m, Monad m) => Var -> m Var
fillVar x
  | isUnderscore x = do
    n <- newUnique
    return $ toSymbol ("_x" ++ show n)
  | otherwise = return x

fillUnderscoresE :: (UniqueGen m, Monad m) => Exp -> m Exp
fillUnderscoresE (ESucc e  ) = fillUnderscoresE e >>= \e' -> return (ESucc e')
fillUnderscoresE (EFunc x c) = do
  z <- fillVar x
  fillUnderscoresC c >>= \c' -> return (EFunc z c')
fillUnderscoresE (EHand x cv cls) = do
  z    <- fillVar x
  cv'  <- fillUnderscoresC cv
  cls' <- mapM
    (\(op, x, k, ci) -> do
      x'  <- fillVar x
      k'  <- fillVar k
      ci' <- fillUnderscoresC ci
      return (op, x', k', ci')
    )
    cls
  return (EHand z cv' cls')
fillUnderscoresE (EAnno e t) = do
  e' <- fillUnderscoresE e
  return (EAnno e' t)
fillUnderscoresE e = return e

fillUnderscoresC :: (UniqueGen m, Monad m) => Comp -> m Comp
fillUnderscoresC (CVal e      ) = fillUnderscoresE e >>= \e' -> return (CVal e')
fillUnderscoresC (COp op e y c) = do
  z  <- fillVar y
  e' <- fillUnderscoresE e
  c' <- fillUnderscoresC c
  return (COp op e' z c')
fillUnderscoresC (CWith e c) = do
  e' <- fillUnderscoresE e
  c' <- fillUnderscoresC c
  return (CWith e' c')
fillUnderscoresC (CApp e1 e2) = do
  e1' <- fillUnderscoresE e1
  e2' <- fillUnderscoresE e2
  return (CApp e1' e2')
fillUnderscoresC (CIf e c1 c2) = do
  e'  <- fillUnderscoresE e
  c1' <- fillUnderscoresC c1
  c2' <- fillUnderscoresC c2
  return (CIf e' c1' c2')
fillUnderscoresC (CMatch e c1 x c2) = do
  z   <- fillVar x
  e'  <- fillUnderscoresE e
  c1' <- fillUnderscoresC c1
  c2' <- fillUnderscoresC c2
  return (CMatch e' c1' z c2')
fillUnderscoresC (CLet x c1 c2) = do
  z   <- fillVar x
  c1' <- fillUnderscoresC c1
  c2' <- fillUnderscoresC c2
  return (CLet z c1' c2')
fillUnderscoresC (CAnno c t) = do
  c' <- fillUnderscoresC c
  return (CAnno c' t)

fillUnderscores :: (UniqueGen m, Monad m) => Term -> m Term
fillUnderscores (E e) = fillUnderscoresE e >>= \e' -> return (E e')
fillUnderscores (C c) = fillUnderscoresC c >>= \c' -> return (C c')

fillUnderscoresDecs :: (UniqueGen m, Monad m) => [Dec] -> m [Dec]
fillUnderscoresDecs []                      = return []
fillUnderscoresDecs ((id, tau, def) : rest) = do
  def'  <- fillUnderscores def
  rest' <- fillUnderscoresDecs rest
  return ((id, tau, def') : rest')

-- | Desugar a list of declarations into a nested let, as explained before. 
-- Declarations are assumed valid.
desugarDecs :: [Dec] -> (Type, Term)
desugarDecs decs = (getMainType decs, desugarDecs' decs)
 where
  getMainType decs = (\(_, tmain, _) -> tmain) (last decs)
  toValT (VT t) = t
  toCT (CT t) = t
  toExp (E e) = e
  toComp (C c) = c
  desugarDecs' :: [Dec] -> Term
  desugarDecs' [(_, tau, main)       ] = C $ CAnno (toComp main) (toCT tau)
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
    decs' <- fillUnderscoresDecs decs
    let sig             = sigma ++ fromMaybe [] sigM
    let (dsTau, dsTerm) = desugarDecs decs'
    s <- checkType (initEnv sig) dsTerm dsTau
    return (dsTau, dsTerm, s, sig)

-- | Run a computation term with initial env.
run :: Term -> Type -> Sig -> Either Error Comp
run (C c) (CT ct) sig = runSteps (initEnv sig) (c, Just ct)
