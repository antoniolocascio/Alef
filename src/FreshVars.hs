-- Used only to define type class FreshGen as a synonym of UniqueGen.
{-# LANGUAGE UndecidableInstances #-}

module FreshVars where

import           Utils.Unique
import           Utils.Symbol

class FreshGen m where
  newFreshVar :: m EffVar

instance (UniqueGen m, Monad m) => FreshGen m where
  newFreshVar = do
    u <- newUnique
    return $ toSymbol ("_mu" ++ show u)
