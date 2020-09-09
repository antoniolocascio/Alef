module Types where

import           Utils.Substitution
import           Utils.Symbol                   ( EffVar )
import           Utils.Set                     as S

import           EffectRow

-- | A type can be either a value type or a computation type. 
data Type = VT VType | CT CType  deriving (Eq)
data TypeScheme = TS (S.Set EffVar) Type
  deriving Eq

-- | Unwrap value type
toVT :: Type -> VType
toVT (VT t) = t

-- | Unwrap computation type
toCT :: Type -> CType
toCT (CT t) = t

-- | Determine if a type is a value type.
isValue :: Type -> Bool
isValue (VT _) = True
isValue _      = False

-- | Determine if a type is a computation type.
isComp :: Type -> Bool
isComp (CT _) = True
isComp _      = False

-- | Internal representation of value types.
data VType where
    -- | Base types
    TBool ::VType
    TNat ::VType
    TUnit ::VType
    TEmpty ::VType
    -- | Function type
    TFunc ::VType -> CType -> VType
    -- | Handler type
    THand ::CType -> CType -> VType
 deriving (Eq)

-- | Internal representation of computation types.
data CType where TComp ::VType -> EffRow -> CType
 deriving (Eq)

-- | Operation types
data OpType where TOp ::VType -> VType -> OpType
 deriving (Eq)

-- | Substitutable instance for VType
instance Substitutable VType EffVar EffRow where
  apply sub (TFunc a c) = TFunc (apply sub a) (apply sub c)
  apply sub (THand c d) = THand (apply sub c) (apply sub d)
  apply sub vt          = vt

-- | Substitutable instance for THand
instance Substitutable CType EffVar EffRow where
  apply sub (TComp vt er) = TComp (apply sub vt) (apply sub er)

-- | Substitutable instance for Type 
instance Substitutable Type EffVar EffRow  where
  apply sub (VT vt) = VT $ apply sub vt
  apply sub (CT ct) = CT $ apply sub ct

-- | Substitutable instance for Type Scheme
instance Substitutable TypeScheme EffVar EffRow where
  apply sub (TS bv pt) =
    TS bv (apply (dres sub (S.toList $ S.diff (S.fromList $ dom sub) bv)) pt)

instance Substitutable (Either Type TypeScheme) EffVar EffRow where
  apply sub (Left  t ) = Left $ apply sub t
  apply sub (Right ts) = Right $ apply sub ts

-- | Substitutable instance for OpType
instance Substitutable OpType EffVar EffRow where
  apply sub (TOp a b) = TOp (apply sub a) (apply sub b)

-- | List of effect variables present in a type 
fv :: Type -> [EffVar]
fv = S.toList . fv_
 where
  fv_ (VT vt) = fvVT vt
  fv_ (CT ct) = fvCT ct
  fvVT (TFunc a c) = S.union (fvVT a) (fvCT c)
  fvVT (THand c d) = S.union (fvCT c) (fvCT d)
  fvVT _           = S.empty
  fvCT (TComp a er) = S.insert (effVar er) (fvVT a)

fvTS :: TypeScheme -> [EffVar]
fvTS (TS bv pt) = S.toList $ S.diff (S.fromList $ fv pt) bv
