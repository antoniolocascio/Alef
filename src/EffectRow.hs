module EffectRow where

import           Prelude                 hiding ( lookup )
import           Data.Maybe                     ( fromMaybe )

import           Utils.Set                     as Set
import           Utils.Substitution            as Sub
import           Utils.Symbol                   ( EffVar )

import           Operation


-- | Delta is just a type synonym of Set Op
type Delta = Set.Set Operation

-- | Effect rows have a set containing its operations and an effect variable. 
type EffRow = (Delta, EffVar)

-- | Create empty row with the given variable
emptyRow :: EffVar -> EffRow
emptyRow mu = (Set.empty, mu)

-- | Return a row's effect variable
effVar :: EffRow -> EffVar
effVar (_, mu) = mu

--- | Return a list of a row's operations 
operations :: EffRow -> [Operation]
operations (d, _) = Set.toList d

-- | Add an operation to a row. 
addOp :: Operation -> EffRow -> EffRow
addOp op (d, mu) = (Set.insert op d, mu)

-- | Check if an operation is part of a row.
hasOp :: Operation -> EffRow -> Bool
hasOp op (d, _) = Set.elem op d

-- | Make a row from a list of ops and a variable. 
makeRow :: [Operation] -> EffVar -> EffRow
makeRow ops mu = (Set.fromList ops, mu)

-- | Check is a substitution is just a renaming.
renaming :: EffRow -> Bool
renaming (d, _) = Set.isEmpty d

-- | Substitutable instance for EffRow
instance Substitutable EffRow EffVar EffRow where
  apply sub (d, mu) = case lookup mu sub of
    Just (d', mu') -> (Set.union d d', mu')
    Nothing        -> (d, mu)

toRenaming :: Substitution EffVar EffRow -> Substitution EffVar EffRow
toRenaming s = Sub.unlist $ (\(mu, r) -> (mu, emptyRow (effVar r))) <$> list s
