{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module AST where

import           Prelude                 hiding ( lookup )
import           Data.Maybe                     ( fromMaybe )

import           Utils.Symbol
import           Utils.Substitution

import           Types
import           Operation
import           EffectRow


-- | A term can be a pure expression or an effectful computation.
data Term = E Exp | C Comp deriving (Eq)

-- | Expressions 
data Exp where
  -- | Variables
  EVar ::Var -> Exp
  -- | Constants
  ETrue ::Exp
  EFalse ::Exp
  EZero ::Exp
  ESucc ::Exp -> Exp
  EUnit ::Exp
  -- | Functions
  EFunc ::Var -> Comp -> Exp
  -- | Handlers
  EHand ::Var  -- val x
          -> Comp -- cv
          -> [(Operation -- op
              , Var -- x 
              , Var -- k
              , Comp -- ci
              )]
          -> Exp
  -- | Type annotations
  EAnno ::Exp -> VType -> Exp
   deriving (Eq)

-- | Computations              
data Comp where
  -- | Value
  CVal ::Exp -> Comp
  -- | Operation call
  COp ::Operation -> Exp -> Var -> Comp -> Comp
  -- | With e handle c
  CWith ::Exp -> Comp -> Comp
  -- | Function application
  CApp ::Exp -> Exp -> Comp
  -- | If then else
  CIf ::Exp -> Comp -> Comp -> Comp
  -- | Let
  CLet ::Var -> Comp -> Comp -> Comp
  -- | Match
  CMatch ::Exp -> Comp -> Var -> Comp -> Comp
  CAnno ::Comp -> CType -> Comp
 deriving (Eq)

-- | Substitutable instance for Exp EffVar EffRow
instance Substitutable Exp EffVar EffRow where
  apply sub (EFunc v c) = EFunc v (apply sub c)
  apply sub (EHand x cv ops) =
    let cv'  = apply sub cv
        ops' = (\(op, y, k, ci) -> (op, y, k, apply sub ci)) <$> ops
    in  EHand x cv' ops'
  apply sub (ESucc e  ) = ESucc $ apply sub e
  apply sub (EAnno e t) = EAnno (apply sub e) (apply sub t)
  apply _   e           = e

-- | Substitutable instance for Comp EffVar EffRow
instance Substitutable Comp EffVar EffRow where
  apply sub (CVal e      ) = CVal $ apply sub e
  apply sub (COp op e y c) = COp op (apply sub e) y (apply sub c)
  apply sub (CWith h  c  ) = CWith (apply sub h) (apply sub c)
  apply sub (CApp  e1 e2 ) = CApp (apply sub e1) (apply sub e2)
  apply sub (CIf e c1 c2 ) = CIf (apply sub e) (apply sub c1) (apply sub c2)
  apply sub (CMatch e c1 x c2) =
    CMatch (apply sub e) (apply sub c1) x (apply sub c2)
  apply sub (CLet x c1 c2) = CLet x (apply sub c1) (apply sub c2)
  apply sub (CAnno c t   ) = CAnno (apply sub c) (apply sub t)

-- | Substitutable instance for Exp Var Exp
instance Substitutable Exp Var Exp where
  apply sub (EFunc v c) = EFunc v (apply (ignore v sub) c)
  apply sub (EVar v   ) = fromMaybe (EVar v) (lookup v sub)
  apply sub (EHand x cv ops) =
    let cv' = apply (ignore x sub) cv
        ops' =
          (\(op, y, k, ci) -> (op, y, k, apply (ignore y $ ignore k sub) ci))
            <$> ops
    in  EHand x cv' ops'
  apply sub (ESucc e  ) = ESucc $ apply sub e
  apply sub (EAnno e t) = EAnno (apply sub e) t
  apply _   e           = e

-- | Substitutable instance for Comp Var Exp
instance Substitutable Comp Var Exp where
  apply sub (CVal e      ) = CVal $ apply sub e
  apply sub (COp op e y c) = COp op (apply sub e) y (apply (ignore y sub) c)
  apply sub (CWith h  c  ) = CWith (apply sub h) (apply sub c)
  apply sub (CApp  e1 e2 ) = CApp (apply sub e1) (apply sub e2)
  apply sub (CIf e c1 c2 ) = CIf (apply sub e) (apply sub c1) (apply sub c2)
  apply sub (CMatch e c1 x c2) =
    CMatch (apply sub e) (apply sub c1) x (apply (ignore x sub) c2)
  apply sub (CLet x c1 c2) = CLet x (apply sub c1) (apply (ignore x sub) c2)
  apply sub (CAnno c t   ) = CAnno (apply sub c) t

-- | Ignore a variable in a substitution. Used for bound variables.
ignore :: Var -> Substitution Var Exp -> Substitution Var Exp
ignore v = insert v (EVar v)

-- | Substitutable instance for Term 
instance (Substitutable Exp a b, Substitutable Comp a b) => Substitutable Term a b where
  apply sub (E e) = E $ apply sub e
  apply sub (C c) = C $ apply sub c
