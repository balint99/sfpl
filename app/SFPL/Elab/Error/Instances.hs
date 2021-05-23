{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}

-- | Instances for types describing elaboration errors.
module SFPL.Elab.Error.Instances where

import SFPL.Base
import SFPL.Elab.Instances
import SFPL.Elab.Error.Types
import SFPL.Elab.Error.Pretty

-- | @since 1.0.0
instance Pretty ErrorPCxt ElabError where
  pretty = prettyElabError

deriving instance Show ElabError                -- ^ @since 1.0.0
deriving instance Show ElabErrorType            -- ^ @since 1.0.0
deriving instance Show SyntacticCategory        -- ^ @since 1.0.0
deriving instance Show MalformedTypeErrorReason -- ^ @since 1.0.0
deriving instance Show TypeHolePlace            -- ^ @since 1.0.0
deriving instance Show UnificationErrorReason   -- ^ @since 1.0.0
deriving instance Show OverloadType             -- ^ @since 1.0.0
deriving instance Show ElabErrorItem            -- ^ @since 1.0.0
