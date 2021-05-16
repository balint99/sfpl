
module SFPL.Elab.Class where

import GHC.Stack
import SFPL.Elab.Metacontext

-- | Elaboration monad class.
--
-- @since 1.0.0
class MonadMeta m => MonadElab m where
