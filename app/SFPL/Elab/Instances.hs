{-# LANGUAGE StandaloneDeriving #-}

-- Instances for types needed for elaboration.
module SFPL.Elab.Instances where

import SFPL.Elab.Context

deriving instance Show TopLevelCxt  -- ^ @since 1.0.0
deriving instance Show Namespaces   -- ^ @since 1.0.0
deriving instance Show TyEntry      -- ^ @since 1.0.0
deriving instance Show TmEntry      -- ^ @since 1.0.0
deriving instance Show PrintCxt     -- ^ @since 1.0.0
deriving instance Show ElabCxt      -- ^ @since 1.0.0
