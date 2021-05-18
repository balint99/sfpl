
module SFPL.Elab.Class where

import SFPL.Elab.Context
import SFPL.Elab.Error
import SFPL.Elab.Metacontext
import SFPL.Eval.Types (VTy)
import SFPL.Syntax.Raw.Types (Span)

-- | Elaboration monad class.
--
-- @since 1.0.0
class MonadMeta m => MonadElab m where
  -- | Get the current elaboration context.
  getElabCxt :: m ElabCxt
  
  -- | Register an elaboration error.
  registerElabError :: ElabError -> m ()
  
  -- | Throw the registered elaboration errors.
  throwElabErrors :: m a
