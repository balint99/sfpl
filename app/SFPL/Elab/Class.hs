
module SFPL.Elab.Class where

import SFPL.Elab.Error
import SFPL.Elab.Metacontext

-- | Elaboration monad class.
--
-- @since 1.0.0
class MonadMeta m => MonadElab m where
  -- | Get the current elaboration context.
  getElabCxt :: m ElabCxt
  
  -- | Throw an elaboration error.
  throwElabError :: ElabError -> m a
  
  -- | Register a term hole, using the given expected type.
  registerHole :: Ty -> m ()
