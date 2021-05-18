{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase #-}
-- | Elaboration.
module SFPL.Elab
  ( module SFPL.Elab,
    module SFPL.Elab.Error,
    module SFPL.Elab.Metacontext,
    module SFPL.Elab.Class,
  )
  where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Control.Monad.State
import Control.Monad.Except
import SFPL.Base
import SFPL.Utils
import SFPL.Elab.Metacontext
import SFPL.Syntax.Core
import SFPL.Elab.Error
import SFPL.Elab.Class
import SFPL.Elab.Unification
import SFPL.Eval.Types
import Text.PrettyPrint
import Prelude hiding ((<>))

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

getMetasDefault :: MonadState Metacxt m => m SomeMetas
getMetasDefault = SomeMetas . entries <$> get

instance Metas Metacxt where
  metasGet m = metasGet m . entries
  metasToAssocList f = metasToAssocList f . entries

type Elab = State Metacxt

instance MonadMeta Elab where
  freshMeta = freshMetaDefault
  lookupMeta = lookupMetaDefault
  updateMeta = updateMetaDefault
  getMetas = getMetasDefault

showReason :: UnificationErrorReason -> String
showReason = \case
  RigidMismatch       -> "rigid mismatch"
  InvalidSpine        -> "invalid spine"
  EscapingVariable l  -> "escaping variable: " ++ show l
  Occurs m            -> "occurs check failed for: " ++ show m

mcxt1 = Metacxt M.empty 0
freshMetas = (\x -> freshMeta (MetaInfo 2 x)) <$$> map (\c -> [c, '0']) ['a'..'d']
env = [VTyVar 1, VTyVar 0]

testUnify :: Lvl -> Elab VTy -> Elab VTy -> IO ()
testUnify n va vb =
  let (r, mcxt') = runState (freshMetas >> runExceptT (bindM2 (unify n) (lift va) (lift vb))) mcxt1
  in putStrLn $ either showReason (const $ showMetacxt mcxt') r
  where
    showMetacxt (Metacxt entries _) = 
      let assocs = M.toList entries
          xs = ["b", "a"]
          tcxt = tyPCxt xs (map (fmap $ metaName . snd) assocs) ["list"]
          showEntry (st, info) = render $
            text (metaName info) <> text " = " <> case st of
              Solved a  -> pretty tcxt a
              Unsolved  -> char '?'
      in  unlines $ map (\(m, entry) -> showEntry entry) assocs
