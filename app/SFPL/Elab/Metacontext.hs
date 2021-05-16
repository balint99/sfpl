{-# LANGUAGE FlexibleContexts #-}

-- | Types and functions corresponding to the metacontext of elaboration.
module SFPL.Elab.Metacontext
  ( MetaState (..),
    MetaInfo (..),
    MetaEntry,
    MonadMeta (..),
    Metacxt (..),
    freshMetaDefault,
    lookupMetaDefault,
    updateMetaDefault,
  )
  where

import Control.Monad.State
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import GHC.Stack
import SFPL.Base
import SFPL.Syntax.Core.Types
import SFPL.Utils

-- | The state of a metavariable.
--
-- @since 1.0.0
data MetaState
  = -- | A solved meta.
    Solved Ty
  | -- | An unsolved meta.
    Unsolved

-- | Information about a metavariable.
--
-- @since 1.0.0
data MetaInfo = MetaInfo
  { -- | The size of the metavariable's context.
    metaLvl :: Lvl
  , -- | The metavariable's name.
    metaName :: TyName
  }

-- | An entry in the metacontext.
--
-- @since 1.0.0
type MetaEntry = (MetaState, MetaInfo)

-- | Type class for monads with a metacontext.
--
-- @since 1.0.0
class Monad m => MonadMeta m where
  -- | Create a fresh metavariable.
  freshMeta :: MetaInfo -> m Ty
  
  -- | Lookup a metavariable in the metacontext.
  -- Precondition: the metavariable exists.
  lookupMeta :: HasCallStack => Metavar -> m MetaEntry
  
  -- | Update a metavariable with a solution.
  -- Precondition: the metavariable exists.
  updateMeta :: HasCallStack => Metavar -> Ty -> m ()

-- | The metacontext.
--
-- @since 1.0.0
data Metacxt = Metacxt
  { -- | The current metavariables.
    entries :: HashMap Metavar MetaEntry
  , -- | The ordinal of the next metavariable.
    nextMeta :: Metavar
  }

-- Default methods
freshMetaDefault :: MonadState Metacxt m => MetaInfo -> m Ty
freshMetaDefault info = do
  mcxt <- get
  let m = nextMeta mcxt
      newEntries = M.insert m (Unsolved, info) $ entries mcxt
  put $ Metacxt newEntries (m + 1)
  pure $ FreshMeta m

lookupMetaDefault :: WithCS (MonadState Metacxt m) =>
  Metavar -> m MetaEntry
lookupMetaDefault m = do
  mcxt <- get
  case M.lookup m (entries mcxt) of
    Just entry  -> pure entry
    Nothing     -> devError $ "metavariable not in scope: " ++ show m

updateMetaDefault :: WithCS (MonadState Metacxt m) =>
  Metavar -> Ty -> m ()
updateMetaDefault m a = do
  mcxt <- get
  if M.member m (entries mcxt)
    then modify (putSolution $ Solved a)
    else devError $ "metavariable not in scope: " ++ show m
  where
    putSolution sol mcxt = mcxt {entries = M.adjust f m $ entries mcxt}
      where
        f (_, info) = (sol, info)
