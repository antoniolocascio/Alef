-- Used only to define type class SubsIndex as a synonym of Ord.
{-# LANGUAGE UndecidableInstances #-}

module Utils.Substitution where

import           Prelude                 hiding ( lookup )
import qualified Data.Map.Strict               as M

import           Utils.Symbol                  as Sym
                                                ( EffVar )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.List                     as L
import qualified Data.Set                      as S

-- | Type class of possible substitution index types. 
-- Ord constraint should be enough.
class SubsIndex k where
  data Substitution k :: * -> *
  empty :: Substitution k v
  lookup :: k -> Substitution k v -> Maybe v
  insert :: k -> v -> Substitution k v -> Substitution k v
  (°) :: (Substitutable v k v) => Substitution k v
                               -> Substitution k v
                               -> Substitution k v
  (°^) :: (Substitutable v k v) => Substitution k v
                               -> Substitution k v
                               -> Substitution k v
  ri :: (Eq v) => Substitution k v -> (v -> Bool) -> Bool
  dres :: Substitution k v -> [k] -> Substitution k v
  list :: Substitution k v -> [(k, v)]
  unlist :: [(k, v)] -> Substitution k v
  dom :: Substitution k v -> [k]
  union ::  Substitution k v -> Substitution k v -> Substitution k v

class Substitutable e k v where
  apply :: Substitution k v -> e -> e

-- | Every instance of Ord has a default instance of SubsIndex
instance Ord k => SubsIndex k  where
  data Substitution k v        = SubsK (M.Map k v)
  empty = SubsK M.empty
  lookup k (SubsK m) = M.lookup k m
  insert k v (SubsK m) = SubsK (M.insert k v m)
  (SubsK m1) ° s2@(SubsK m2) =
    let m3 = apply s2 <$> m1 in SubsK $ M.union m3 m2
  s1@(SubsK m1) °^ s2@(SubsK m2) =
    let m3 = apply s2 <$> m1 in SubsK $ M.union m2 m3
  ri (SubsK m1) isVar = let l = M.elems m1 in L.nub l == l && all isVar l
  dres (SubsK m) doml =
    let doms = S.fromList doml in SubsK $ M.restrictKeys m doms
  list (SubsK m) = M.toList m
  unlist l = SubsK (M.fromList l)
  dom (SubsK m) = M.keys m
  union (SubsK m1) (SubsK m2) = SubsK $ M.union m1 m2

instance Substitutable EffVar EffVar EffVar where
  apply sub v = fromMaybe v (lookup v sub)
