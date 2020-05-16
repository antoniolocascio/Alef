module RowUnification where

import           EffectRow
import           Types
import           Utils.Substitution
import           Utils.Symbol
import           Utils.Set                      ( diff
                                                , symDiff
                                                )
import           Utils.Error
import           FreshVars
import           AST

-- | Effect variable instantiation for rows.
instRows
  :: (FreshGen m, Monad m) => EffRow -> EffRow -> m (Substitution EffVar EffRow)
instRows (d1, mu1) (d2, mu2)
  | mu1 == mu2 = do
    mu' <- newFreshVar
    return $ insert mu1 (d1 `symDiff` d2, mu') empty
  | otherwise = do
    mu' <- newFreshVar
    let diff1 = diff d2 d1
        diff2 = diff d1 d2
    return $ insert mu1 (diff1, mu') $ insert mu2 (diff2, mu') empty


-- | Effect variable instantiation for types. 
instTypes
  :: (FreshGen m, Monad m, Fallible m)
  => Type
  -> Type
  -> Term
  -> m (Substitution EffVar EffRow)
instTypes (VT TBool) (VT TBool) _ = return empty
instTypes (VT TNat) (VT TNat) _ = return empty
instTypes (VT TUnit) (VT TUnit) _ = return empty
instTypes (VT TEmpty) (VT TEmpty) _ = return empty
instTypes t1@(VT (TFunc a (TComp c e))) t2@(VT (TFunc a' (TComp c' e'))) t = do
  s1 <- instTypes (VT a) (VT a') t
  s2 <- instTypes (apply s1 (VT c)) (apply s1 (VT c')) t
  s3 <- instRows (apply (s1 ° s2) e) (apply (s1 ° s2) e')
  return ((s1 ° s2) ° s3)
instTypes (VT (THand (TComp c e1) (TComp d e2))) (VT (THand (TComp c' e1') (TComp d' e2'))) t
  = do
    s1 <- instTypes (VT c) (VT c') t
    s2 <- instTypes (apply s1 (VT d)) (apply s1 (VT d')) t
    s3 <- instRows (apply (s1 ° s2) e1) (apply (s1 ° s2) e1')
    s4 <- instRows (apply ((s1 ° s2) ° s3) e2) (apply ((s1 ° s2) ° s3) e2')
    return $ ((s1 ° s2) ° s3) ° s4
instTypes (CT (TComp c e)) (CT (TComp c' e')) t = do
  s1 <- instTypes (VT c) (VT c') t
  s2 <- instRows (apply s1 e) (apply s1 e')
  return $ s1 ° s2
instTypes t1 t2 t = throw $ incompatibleTypes t1 t2 t

