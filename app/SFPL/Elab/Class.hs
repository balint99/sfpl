{-# LANGUAGE KindSignatures #-}

-- | Elaboration monad class.
module SFPL.Elab.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Kind
import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Error
import SFPL.Elab.Metacontext

-- | Type class for describing monads in which
-- elaboration can be carried out.
--
-- @since 1.0.0
class MonadMeta m => MonadElab (m :: Type -> Type) where
  -- | Get the current elaboration context.
  getElabCxt :: m ElabCxt
  
  -- | Modify the elaboration context.
  withElabCxt :: (ElabCxt -> ElabCxt) -> m a -> m a
  
  -- | Register an elaboration error.
  registerElabError :: ElabError -> m ()
  
  -- | Query if there are any registered errors.
  isErrorRegistered :: m Bool
  
  -- | Throw the registered elaboration errors.
  -- Precondition: there is at least 1 registered error.
  throwElabErrors :: m a
  
  -- | Find a fresh name for a metavariable using the given
  -- base name.
  freshName :: TyName -> m TyName

-- | @since 1.0.0
instance MonadElab m => MonadElab (MaybeT m) where
  getElabCxt = lift getElabCxt
  withElabCxt = mapMaybeT . withElabCxt
  registerElabError = lift . registerElabError
  isErrorRegistered = lift isErrorRegistered
  throwElabErrors = lift throwElabErrors
  freshName = lift . freshName
