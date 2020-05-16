module Utils.Set
  ( Set
  , insert
  , elem
  , diff
  , union
  , fromList
  , toList
  , empty
  , isEmpty
  , subset
  , symDiff
  )
where

import           Prelude                       as P
                                         hiding ( elem )
import qualified Data.Set                      as S

-- | Representation of sets.
type Set = S.Set

insert :: Ord a => a -> Set a -> Set a
insert = S.insert

elem :: Ord a => a -> Set a -> Bool
elem = S.member

diff :: Ord a => Set a -> Set a -> Set a
diff = S.difference

union :: Ord a => Set a -> Set a -> Set a
union = S.union

symDiff :: Ord a => Set a -> Set a -> Set a
symDiff a b = (a `S.difference` b) `S.union` (b `S.difference` a)

fromList :: Ord a => [a] -> Set a
fromList = S.fromList

toList :: Ord a => Set a -> [a]
toList = S.toList

empty :: Ord a => Set a
empty = S.empty

isEmpty :: Ord a => Set a -> Bool
isEmpty = S.null

subset :: Ord a => Set a -> Set a -> Bool
subset = S.isSubsetOf
