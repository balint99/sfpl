{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module SFPL.Elab.Error.Instances where

import SFPL.Base
import SFPL.Elab.Error.Types
import SFPL.Elab.Error.Pretty

-- | @since 1.0.0
instance Pretty ErrorPCxt ElabError where
  pretty = prettyElabError
