{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- | Types and functions corresponding to the metacontext of elaboration.
module SFPL.Elab.Metacontext
  ( -- * Types
    MetaState (..),
    MetaInfo (..),
    MetaEntry,
    SomeMetas (..),
    
    -- * Classes
    Metas (..),
    MonadMeta (..),
  )
  where

import Control.Monad.State
import Data.Array.IArray (IArray)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import GHC.Stack
import SFPL.Base
import SFPL.Syntax.Core.Types
import SFPL.Utils

------------------------------------------------------------
-- Types

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

-- | Abstract type for metavariable mappings.
--
-- @since 1.0.0
data SomeMetas = forall ms. Metas ms => SomeMetas ms

------------------------------------------------------------
-- Classes

-- | A type class describing metavariable mappings. It is useful for
-- taking a snapshot of the current metavariables and then
-- accessing the information about them efficiently.
--
-- @since 1.0.0
class Metas ms where
  -- | Lookup a metavariable.
  getMeta :: Metavar -> ms -> MetaEntry
  
  -- | Transform the mappings to an immutable array.
  toArray :: IArray a e => (MetaEntry -> e) -> ms -> a Metavar e

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
  
  -- | Get the current metavariable mappings.
  getMetas :: m SomeMetas
{-
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
-}
