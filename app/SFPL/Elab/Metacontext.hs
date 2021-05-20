{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- | Types and functions corresponding to the metacontext of elaboration.
module SFPL.Elab.Metacontext
  ( -- * Types
    MetaState (..),
    MetaInfo (..),
    MetaEntry,
    SomeMetas (..),
    getMeta,
    toAssocList,
    
    -- * Classes
    Metas (..),
    MonadMeta (..),
  )
  where

import Control.Monad.State
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

-- | Lookup a metavariable.
--
-- @since 1.0.0
getMeta :: Metavar -> SomeMetas -> MetaEntry
getMeta m (SomeMetas metas) = metasGet m metas

-- | Transform the mappings to an association list,
-- applying the given function to the entries.
--
-- @since 1.0.0
toAssocList :: (MetaEntry -> e) -> SomeMetas -> [(Metavar, e)]
toAssocList f (SomeMetas metas) = metasToAssocList f metas

------------------------------------------------------------
-- Classes

-- | A type class describing metavariable mappings. It is useful for
-- taking a snapshot of the current metavariables and then
-- accessing the information about them efficiently.
--
-- @since 1.0.0
class Metas ms where
  -- | Lookup a metavariable.
  metasGet :: Metavar -> ms -> MetaEntry
  
  -- | Transform the mappings to an association list,
  -- applying the given function to the entries.
  metasToAssocList :: (MetaEntry -> e) -> ms -> [(Metavar, e)]

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
