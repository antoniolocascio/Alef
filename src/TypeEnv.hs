{-# LANGUAGE FlexibleContexts #-}

module TypeEnv where

import           Types
import           Utils.Symbol
import           Utils.Error
import           Utils.Substitution
import           Operation
import           EffectRow
import qualified Data.Map.Strict               as M

-- | Type class of typing environments.
class (Substitutable e EffVar EffRow) => TypeEnv e where
  lookupVar :: Fallible m => EffVar -> e -> m Type
  lookupOp :: Fallible m => Operation -> e -> m OpType
  extEnv :: EffVar -> Type -> e -> e
  vars :: e -> [(EffVar, Type)]
  ops :: e -> [(Operation, OpType)]

-- | Concrete type environment.
data EnvC = Env {vEnv :: M.Map EffVar Type, opEnv :: M.Map Operation OpType}

initEnv :: [(Operation, OpType)] -> EnvC
initEnv sigma = Env { vEnv = M.empty, opEnv = M.fromList sigma }

instance Substitutable v EffVar EffRow => Substitutable (M.Map k v) EffVar EffRow where
  apply sub = M.map (apply sub)

instance Substitutable EnvC EffVar EffRow where
  apply sub env =
    Env { vEnv = apply sub (vEnv env), opEnv = apply sub (opEnv env) }

instance TypeEnv EnvC where
  lookupVar v env = case M.lookup v (vEnv env) of
    Just t -> return t
    Nothing ->
      throw $ "Variable name: " ++ show v ++ " not found in environment."
  lookupOp op env = case M.lookup op (opEnv env) of
    Just t -> return t
    Nothing ->
      throw $ "Operation name: " ++ show op ++ " not found in environment."
  extEnv x t env = Env { vEnv = M.insert x t (vEnv env), opEnv = opEnv env }
  vars = M.toList . vEnv
  ops  = M.toList . opEnv

