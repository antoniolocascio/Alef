module TypeChecker where

import           Data.Maybe                     ( maybe )
import qualified Data.List                     as L
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Fail
import qualified Data.Map.Strict               as M
import           Data.Bifunctor                 ( first
                                                , second
                                                )

import           Utils.Error
import           Utils.Symbol
import           Utils.Substitution            as Sub
import           Utils.Set
import           Utils.Unique

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST


import           FreshVars
import           TypeEnv
import           AST
import           Types
import           EffectRow
import           RowUnification
import           Operation

class (Monad m, Fallible m, FreshGen m) => TypeChecker m where

-- | Synthesis judgment
synthType
  :: (TypeChecker m, TypeEnv e)
  => e
  -> Term
  -> m (Type, Substitution EffVar EffRow)
-- Var
synthType tenv (E (EVar x)) = do
  t <- lookupVar x tenv
  case t of
    (Left  (VT vt   )) -> return (VT vt, Sub.empty)
    (Right (TS bv pt)) -> do
      rho <- renameBVs (toList bv)
      return (apply rho pt, Sub.empty)

-- Zero
synthType tenv (     E EZero    ) = return (VT TNat, Sub.empty)
-- Succ
synthType tenv term@(E (ESucc e)) = do
  (te, eta) <- synthType tenv (E e)
  case te of
    (VT TNat) -> return (VT TNat, eta)
    _         -> throw $ typeError (VT TNat) te term
-- True
synthType tenv (E ETrue      ) = return (VT TBool, Sub.empty)
-- False
synthType tenv (E EFalse     ) = return (VT TBool, Sub.empty)
-- Unit
synthType tenv (E EUnit      ) = return (VT TUnit, Sub.empty)
-- EAnno
synthType tenv (E (EAnno e t)) = do
  let t' = VT t
  s2 <- checkType tenv (E e) t'
  return (apply s2 t', s2)
-- CAnno
synthType tenv (C (CAnno c t)) = do
  let t' = CT t
  s2 <- checkType tenv (C c) t'
  return (apply s2 t', s2)
-- Val
synthType tenv term@(C (CVal e)) = do
  (te, eta) <- synthType tenv (E e)
  case te of
    (VT t) -> (, eta) . CT . TComp t . emptyRow <$> newFreshVar
    _      -> throw $ expHasCompType term te
-- App
synthType tenv ctx@(C (CApp e1 e2)) = do
  (te1, eta) <- synthType tenv (E e1)
  case te1 of
    (VT (TFunc a c)) -> do
      s <- checkTypeS (apply eta tenv) (E e2) (VT a)
      return (CT (apply s c), eta ° s)
    _ -> throw $ notAFunction (E e1) ctx te1
-- Op
synthType tenv (C (COp op e y c)) = do
  (TOp aop bop) <- lookupOp op tenv
  s             <- checkTypeS tenv (E e) (VT aop)
  (ct, eta)     <- synthType (extEnv y (apply s (Left $ VT bop)) tenv) (C c)
  case ct of
    (CT (TComp a e)) -> do
      mu' <- newFreshVar
      let gamma = Sub.insert (effVar e) (makeRow [op] mu') Sub.empty
      return (CT (apply gamma $ TComp a e), s ° eta ° gamma)
    _ -> throw $ compHasValType (C c) ct
-- With
synthType tenv ctx@(C (CWith h c)) = do
  (th, eta) <- synthType tenv (E h)
  -- id        <- getId
  case th of
    (VT (THand ct dt)) -> do
      s <- checkTypeS (apply eta tenv) (C c) (CT ct)
      return (CT (apply s dt), eta ° s)
    _ -> throw $ notAHandler (E h) ctx th
-- Let
synthType tenv (C (CLet x c1 c2)) = do
  (ta, eta1) <- withId x (synthType tenv (C c1))
  case ta of
    (CT (TComp a e')) -> do
      let tenv' = apply eta1 tenv
          fvs   = fromList (fv_env tenv')
      (tb, eta2) <- synthType
        (extEnv x (Right $ TS (diff (fromList (fv $ VT a)) fvs) (VT a)) tenv')
        (C c2)
      case tb of
        (CT (TComp b e)) -> do
          s <- instRows (apply eta2 e') e
          return (CT (apply s (TComp b e)), eta1 ° eta2 ° s)
        _ -> throw $ compHasValType (C c2) tb
    _ -> throw $ compHasValType (C c1) ta
-- If 
synthType tenv t@(C (CIf e c1 c2)) = do
  checkType tenv (E e) (VT TBool)
  (ct, eta) <- synthType tenv (C c1)
  s         <- checkTypeS (apply eta tenv) (C c2) ct
  return (apply s ct, eta ° s)
-- Match 
synthType tenv (C (CMatch e c1 x c2)) = do
  checkType tenv (E e) (VT TNat)
  (ct, eta) <- synthType tenv (C c1)
  s         <- checkTypeS (extEnv x (Left $ VT TNat) (apply eta tenv)) (C c2) ct
  return (apply s ct, eta ° s)
synthType _ term = throw $ cannotSynth term

-- | Checking judgment with effect variable instantiation
checkTypeS
  :: (TypeChecker m, TypeEnv e)
  => e
  -> Term
  -> Type
  -> m (Substitution EffVar EffRow)
-- Val
checkTypeS tenv (C (CVal e)) (CT (TComp a er)) = checkTypeS tenv (E e) (VT a)
-- Fun
checkTypeS tenv term@(E (EFunc x c)) t = case t of
  VT (TFunc a ct) -> checkTypeS (extEnv x (Left $ VT a) tenv) (C c) (CT ct)
  _               -> throw $ notAFunction term term t
-- Hand
checkTypeS tenv h@(E (EHand x cv opClauses)) t = case t of
  VT (THand (TComp a (dl, mu1)) bt@(TComp b (dl', mu2))) -> do
    checkSameEffVar mu1 mu2 h
    checkDeterministic opClauses
    let hops = fromList $ (\(op, _, _, _) -> op) <$> opClauses
    checkOpsInDeltaPr dl hops dl' h
    s0 <- checkTypeS (extEnv x (Left $ VT a) tenv) (C cv) (CT bt)
    checkOpClauses tenv opClauses bt s0
-- CS
checkTypeS tenv t tau = do
  (tau', eta) <- synthType tenv t
  nameM       <- getId
  sigma       <- instTypes tau' tau (maybe t (E . EVar) nameM)
  return (eta ° sigma)


renameBVs :: TypeChecker m => [EffVar] -> m (Substitution EffVar EffRow)
renameBVs bvs = do
  newVars <- mapM (const newFreshVar) bvs
  return $ foldr (uncurry Sub.insert) Sub.empty (zip bvs (emptyRow <$> newVars))

rename :: TypeChecker m => Type -> m (Type, Substitution EffVar EffRow)
rename (VT vt) = do
  (vt', s) <- renameV vt Sub.empty
  return (VT vt', s)
rename (CT ct) = do
  (ct', s) <- renameC ct Sub.empty
  return (CT ct', s)

renameV
  :: TypeChecker m
  => VType
  -> Substitution EffVar EffRow
  -> m (VType, Substitution EffVar EffRow)
renameV (TFunc a c) s0 = do
  (a', s1) <- renameV a s0
  (c', s2) <- renameC c (s0 ° s1)
  return (TFunc a' c', s0 ° s1 ° s2)
renameV (THand c d) s0 = do
  (c', s1) <- renameC c s0
  (d', s2) <- renameC d (s0 ° s1)
  return (THand c' d', s0 ° s1 ° s2)
renameV vt s = return (vt, s)

renameC
  :: TypeChecker m
  => CType
  -> Substitution EffVar EffRow
  -> m (CType, Substitution EffVar EffRow)
renameC (TComp a er) s0 = do
  (a', s1) <- renameV a s0
  let ops = operations er
      mu  = effVar er
  case Sub.lookup mu (s0 ° s1) of
    Just er' ->
      let mu' = effVar er' in return (TComp a' (makeRow ops mu'), s0 ° s1)
    Nothing -> do
      mu' <- newFreshVar
      return
        (TComp a' (makeRow ops mu'), Sub.insert mu (emptyRow mu') (s0 ° s1))


-- | Convetional checking judgment, the resulting substitution can be ignored.
checkType
  :: (TypeChecker m, TypeEnv e)
  => e
  -> Term
  -> Type
  -> m (Substitution EffVar EffRow)
checkType tenv t tau = do
  s     <- checkTypeS tenv t tau
  nameM <- getId
  if ri (dres s (fv tau)) renaming
    then return s
    else throw $ notAlphaEq tau s (maybe (Right t) Left nameM)

-- | Checking judgment for top-level declarations.
-- Identical to checkType but for additional error information.
-- | Convetional checking judgment, the resulting substitution can be ignored.
checkTypeDec
  :: (TypeChecker m, TypeEnv e)
  => e
  -> Term
  -> Type
  -> Var
  -> m (Substitution EffVar EffRow)
checkTypeDec tenv t tau id = do
  s <- checkTypeS tenv t tau
  if ri (dres s (fv tau)) renaming
    then return s
    else throw $ notAlphaEq tau s (Left id)


-- Helper functions for Hand rule 

-- | Check if every clause handles a different operation. 
checkDeterministic :: TypeChecker m => [(Operation, Var, Var, Comp)] -> m ()
checkDeterministic clauses =
  let opersS = fromList ((\(op, _, _, _) -> op) <$> clauses)
  in  unless (length clauses == length (toList opersS))
             (throw nondeterministicHandler)

-- | Check if two effect variables are equal
checkSameEffVar :: TypeChecker m => EffVar -> EffVar -> Term -> m ()
checkSameEffVar mu1 mu2 h | mu1 == mu2 = return ()
                          | otherwise  = throw $ diffEffVarsHand mu1 mu2 h

-- | Check if every operation not handled by the handler present in delta
-- is also present in delta'
checkOpsInDeltaPr :: TypeChecker m => Delta -> Delta -> Delta -> Term -> m ()
checkOpsInDeltaPr delta hops delta' h
  | subset (diff delta hops) delta' = return ()
  | otherwise                       = throw $ opsNotCapturedHand h

-- Check every operation clause of a handler.
checkOpClauses
  :: (TypeChecker m, TypeEnv e)
  => e
  -> [(Operation, Var, Var, Comp)]
  -> CType
  -> Substitution EffVar EffRow
  -> m (Substitution EffVar EffRow)
checkOpClauses _    []                          _  theta = return theta
checkOpClauses tenv ((opi, x, k, ci) : clauses) bt theta = do
  (TOp ai bi) <- lookupOp opi tenv
  let tenv' = extEnv k (Left $ VT (TFunc bi bt)) (extEnv x (Left $ VT ai) tenv)
  si <- checkTypeS (apply theta tenv') (C ci) (apply theta (CT bt))
  checkOpClauses tenv clauses bt (theta ° si)


-- Concrete instances. 
type St = (Integer, Maybe Symbol)

type TypeCheckerC = StateT St (Except Error)

initialSt :: St
initialSt = (0, Just (toSymbol "main"))

runTC :: TypeCheckerC a -> Either Error a
runTC = runExcept . flip evalStateT initialSt

runCheck :: EnvC -> Term -> Type -> Either Error (Substitution EffVar EffRow)
runCheck tenv t tau = runTC (checkType tenv t tau)

runSynth :: EnvC -> Term -> Either Error (Type, Substitution EffVar EffRow)
runSynth tenv t = runTC (synthType tenv t)

instance Fallible TypeCheckerC where
  throw = throwError
  getId = gets snd
  withId id m = do
    (n, oldId) <- get
    put (n, Just id)
    a       <- m

    (n', _) <- get
    put (n', oldId)
    return a

instance UniqueGen TypeCheckerC where
  newUnique = modify (first (+ 1)) >> gets fst

instance TypeChecker TypeCheckerC where
