{-# LANGUAGE ExistentialQuantification, KindSignatures #-}

-- | Types and functions corresponding to the metacontext of elaboration.
module SFPL.Elab.Metacontext
  ( -- * Types
    MetaState (..),
    MetaInfo (..),
    MetaEntry,
    
    -- ** Abstract metavariables
    SomeMetas,
    someMetas,
    getMeta,
    toAssocList,
    metaNames,
    
    -- * Classes
    Metas (..),
    MonadMeta (..),
  )
  where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Kind
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import GHC.Stack
import SFPL.Base
import SFPL.Syntax.Core

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
  deriving
    ( Show -- ^ @since 1.0.0
    )

-- | Information about a metavariable.
--
-- @since 1.0.0
data MetaInfo = MetaInfo
  { -- | The size of the metavariable's context.
    metaLvl :: Lvl
  , -- | The metavariable's name.
    metaName :: TyName
  }
  deriving
    ( Show -- ^ @since 1.0.0
    )

-- | An entry in the metacontext.
--
-- @since 1.0.0
type MetaEntry = (MetaState, MetaInfo)

----------------------------------------
-- Abstract metavariables

-- | Abstract type for metavariable mappings.
--
-- @since 1.0.0
data SomeMetas = forall ms. Metas ms => SomeMetas ms

-- | Wrap a 'Metas' in 'SomeMetas'.
--
-- @since 1.0.0
someMetas :: Metas ms => ms -> SomeMetas
someMetas = SomeMetas

-- | Lookup a metavariable in a mapping.
--
-- @since 1.0.0
getMeta :: Metavar -> SomeMetas -> MetaEntry
getMeta m (SomeMetas metas) = metasGet m metas

-- | Transform the metavariable mappings to an association list,
-- applying the given function to the entries.
--
-- @since 1.0.0
toAssocList :: (MetaEntry -> e) -> SomeMetas -> [(Metavar, e)]
toAssocList f (SomeMetas metas) = metasToAssocList f metas

-- | Transform the metavariable mappings to a list that associates
-- to each metavariable its name.
--
-- @since 1.0.0
metaNames :: SomeMetas -> [(Metavar, TyName)]
metaNames = toAssocList (metaName . snd)

------------------------------------------------------------
-- Classes

-- | A type class describing metavariable mappings. It is useful for
-- taking a snapshot of the current metavariables and then
-- accessing the information about them efficiently.
--
-- @since 1.0.0
class Metas (ms :: Type) where
  -- | Lookup a metavariable.
  metasGet :: Metavar -> ms -> MetaEntry
  
  -- | Transform the mappings to an association list,
  -- applying the given function to the entries.
  metasToAssocList :: (MetaEntry -> e) -> ms -> [(Metavar, e)]

-- | Type class for monads with a metacontext.
--
-- @since 1.0.0
class Monad m => MonadMeta (m :: Type -> Type) where
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

-- | @since 1.0.0
instance MonadMeta m => MonadMeta (MaybeT m) where
  freshMeta = lift . freshMeta
  lookupMeta = lift . lookupMeta
  updateMeta m = lift . updateMeta m
  getMetas = lift getMetas

-- | @since 1.0.0
instance MonadMeta m => MonadMeta (ExceptT e m) where
  freshMeta = lift . freshMeta
  lookupMeta = lift . lookupMeta
  updateMeta m = lift . updateMeta m
  getMetas = lift getMetas
