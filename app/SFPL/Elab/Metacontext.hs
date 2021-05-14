{-# LANGUAGE FlexibleContexts #-}

-- | Types and functions corresponding to the metacontext of elaboration.
module SFPL.Elab.Metacontext
  ( MetaEntry (..),
    Metacxt (..),
    MonadMeta (..),
    lookupMetaDefault,
    freshMetaDefault,
  )
  where

import Control.Monad.State
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import GHC.Stack
import SFPL.Base
import SFPL.Syntax.Core.Types
import SFPL.Utils

-- | An entry in the metacontext.
--
-- @since 1.0.0
data MetaEntry
  = -- | A solved meta.
    Solved Ty
  | -- | An unsolved meta.
    Unsolved

-- | The metacontext.
--
-- @since 1.0.0
data Metacxt = Metacxt
  { -- | The current metavariables and their solution state.
    entries :: HashMap Metavar MetaEntry
  , -- | The ordinal of the next metavariable.
    nextMeta :: Metavar
  }

-- | Type class for monads with a metacontext.
--
-- @since 1.0.0
class Monad m => MonadMeta m where
  -- | Lookup a metavariable in the metacontext.
  -- Precondition: the metavariable exists.
  lookupMeta :: HasCallStack => Metavar -> m MetaEntry
  
  -- | Create a fresh metavariable.
  freshMeta :: m Ty

-- Default methods
lookupMetaDefault :: WithCS (MonadState Metacxt m) => Metavar -> m MetaEntry
lookupMetaDefault m = do
  mcxt <- get
  case M.lookup m (entries mcxt) of
    Just entry  -> pure entry
    Nothing     -> devError $ "metavariable not in scope: " ++ show m

freshMetaDefault :: MonadState Metacxt m => m Ty
freshMetaDefault = do
  mcxt <- get
  let m = nextMeta mcxt
      newEntries = M.insert m Unsolved $ entries mcxt
  put $ Metacxt newEntries (m + 1)
  pure $ FreshMeta m
