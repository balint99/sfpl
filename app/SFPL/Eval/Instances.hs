{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Instances for types needed for evaluation.
module SFPL.Eval.Instances where

import SFPL.Base
import SFPL.Eval.Types
import SFPL.Eval.Pretty

----------------------------------------
-- Printing

instance Pretty CtrNames Val where
  prettyPrec = prettyVal
