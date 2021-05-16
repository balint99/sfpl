
-- | Pattern unification algorithm.
module SFPL.Elab.Unification where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import SFPL.Base
import SFPL.Elab.Metacontext
import SFPL.Eval.Types
import SFPL.Utils

------------------------------------------------------------
-- Types

data PartialRenaming = PartialRenaming
  { dom :: Lvl
  , cod :: Lvl
  , ren :: HashMap Lvl Lvl
  }
