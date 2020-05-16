module Utils.Unique where

import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.State

-- | We use integers to generate unique values.
type Unique = Integer

-- | Class of unique generators. 
class UniqueGen g where
  newUnique :: g Unique
