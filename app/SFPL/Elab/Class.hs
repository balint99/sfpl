
module SFPL.Elab.Class where

import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Error
import SFPL.Elab.Metacontext

-- | Elaboration monad class.
--
-- @since 1.0.0
class MonadMeta m => MonadElab m where
  -- | Get the current elaboration context.
  getElabCxt :: m ElabCxt
  
  -- | Modify the elaboration context.
  withElabCxt :: (ElabCxt -> ElabCxt) -> m a -> m a
  
  -- | Register an elaboration error.
  registerElabError :: ElabError -> m ()
  
  -- | Query if there are registered term holes.
  isHoleRegistered :: m Bool
  
  -- | Throw the registered elaboration errors.
  -- Precondition: there is at least 1 registered error.
  throwElabErrors :: m a
  
  -- | Find a fresh name for a metavariable, using the given
  -- base name.
  freshName :: TyName -> m TyName
