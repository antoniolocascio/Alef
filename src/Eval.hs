module Eval where

import           Control.Monad.Except
import           Data.Bifunctor                 ( first )
import           Utils.Substitution
import           Utils.Symbol
import           Utils.Error

import           AST
import           Types
import           Operation
import           TypeEnv
import           EffectRow

-- | Small-step semantics.
step :: (Fallible m, TypeEnv e) => e -> Comp -> m Comp
step te (CIf ETrue  c1 _         ) = return c1
step te (CIf EFalse _  c2        ) = return c2
step te (CMatch EZero     c1 _ _ ) = return c1
step te (CMatch (ESucc e) _  x c2) = return $ apply (insert x e empty) c2
step te (CApp (EFunc x c) e      ) = return $ apply (insert x e empty) c
step te (CApp (EAnno (EFunc x c) (TFunc a _)) e) =
  return $ apply (insert x (annotateIfNeeded e a) empty) c
step te (CLet x (CVal e       ) c2) = return $ apply (insert x e empty) c2
step te (CLet x (COp op e y c1) c2) = return $ COp op e y (CLet x c1 c2)
step te (CLet x c1              c2) = do
  c1' <- step te c1
  return $ CLet x c1' c2
step te (CWith (EHand x cv _) (CVal e)) = return $ apply (insert x e empty) cv
step te (CWith (EAnno (EHand x cv _) _) (CVal e)) =
  return $ apply (insert x e empty) cv
step te (CWith h@(EAnno (EHand _ _ clauses) (THand _ bt)) (COp op e y c)) =
  case getClause op clauses of
    Just (_, x, k, ci) -> do
      (TOp _ bi) <- lookupOp op te
      let kt  = EAnno (EFunc y (CWith h c)) (TFunc bi bt)
          sub = insert x e (insert k kt empty)
      return $ apply sub ci
    Nothing -> return $ COp op e y (CWith h c)
step te (CWith h c) = do
  c' <- step te c
  return $ CWith h c'
step te (CAnno c _) = return c

getClause
  :: Operation
  -> [(Operation, Var, Var, Comp)]
  -> Maybe (Operation, Var, Var, Comp)
getClause op [] = Nothing
getClause op (cl@(opi, x, k, ci) : rest) | op == opi = Just cl
                                         | otherwise = getClause op rest

-- | Add annotation to abstractions and handlers.                          
annotateIfNeeded :: Exp -> VType -> Exp
annotateIfNeeded t@EFunc{} tau = EAnno t tau
annotateIfNeeded t@EHand{} tau = EAnno t tau
annotateIfNeeded t         _   = t

-- | Removes unnecesary annotations.                                         
removeAnnosC :: Comp -> Comp
removeAnnosC (CVal e      ) = CVal (removeAnnosE e)
removeAnnosC (COp op e y c) = COp op (removeAnnosE e) y (removeAnnosC c)
removeAnnosC (CWith e  c  ) = CWith (removeAnnosE e) (removeAnnosC c)
removeAnnosC (CApp  e1 e2 ) = CApp (removeAnnosE e1) (removeAnnosE e2)
removeAnnosC (CIf e c1 c2) =
  CIf (removeAnnosE e) (removeAnnosC c1) (removeAnnosC c2)
removeAnnosC (CLet x c1 c2) = CLet x (removeAnnosC c1) (removeAnnosC c2)
removeAnnosC (CMatch e c1 x c2) =
  CMatch (removeAnnosE e) (removeAnnosC c1) x (removeAnnosC c2)
removeAnnosC (CAnno c _) = removeAnnosC c

removeAnnosE :: Exp -> Exp
removeAnnosE (ESucc e  ) = ESucc $ removeAnnosE e
removeAnnosE (EFunc x c) = EFunc x (removeAnnosC c)
removeAnnosE (EHand x cv cls) =
  let cls' = map (\(op, x, k, ci) -> (op, x, k, removeAnnosC ci)) cls
  in  EHand x (removeAnnosC cv) cls'
removeAnnosE t@(EAnno EFunc{} _) = t
removeAnnosE t@(EAnno EHand{} _) = t
removeAnnosE (  EAnno t       _) = t
removeAnnosE t                   = t

-- Step until value or op                                         
steps :: (Fallible m, TypeEnv e) => e -> (Comp, Maybe CType) -> m Comp
steps et (CVal e  , _      ) = return (CVal e)
steps et (op@COp{}, Nothing) = return op
steps et (opc@(COp op _ _ _), Just (TComp _ er))
  | hasOp op er = return opc
  | otherwise   = throw "Operation not in type."
steps et (c, tau) = do
  c' <- step et c
  steps et (c', tau)


-- | Concrete Evaluator for small-step semantics. 
type SmallStepperC = Except Error

runSC :: SmallStepperC a -> Either Error a
runSC = runExcept

runStep :: EnvC -> Comp -> Either Error Comp
runStep e = runExcept . step e . removeAnnosC

--  

runSteps :: EnvC -> (Comp, Maybe CType) -> Either Error Comp
runSteps e = runExcept . steps e . first removeAnnosC

instance Fallible SmallStepperC where
  throw = throwError
