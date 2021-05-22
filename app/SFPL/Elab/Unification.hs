{-# LANGUAGE LambdaCase, RankNTypes #-}

-- | Pattern unification algorithm.
module SFPL.Elab.Unification where

import Control.Monad.Except
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import SFPL.Base
import SFPL.Elab.Error.Types (UnificationErrorReason (..))
import SFPL.Elab.Metacontext
import qualified SFPL.Eval as E (forceTy, ($$$))
import SFPL.Eval.Types
import SFPL.Syntax.Core.Types
import SFPL.Utils

infixl 9 $$$

-- | A partial variable renaming.
data PartialRenaming = PartialRenaming
  { -- | The size of the domain context, where the renaming maps to.
    dom :: Lvl
  , -- | The size of the codomain context, where the renaming maps from.
    cod :: Lvl
  , -- | The renaming itself, mapping de-Bruijn levels of the codomain context
    -- to de-Bruijn levels of the domain context.
    ren :: HashMap Lvl Lvl
  }
  deriving
    ( Show -- ^ @since 1.0.0
    )

-- | Lift a partial renaming to include an additional bound variable.
liftPR :: PartialRenaming -> PartialRenaming
liftPR (PartialRenaming dom cod ren) =
  PartialRenaming (dom + 1) (cod + 1) (M.insert cod dom ren)

-- | Unification monad.
type Unify a = forall m. MonadMeta m => ExceptT UnificationErrorReason m a

-- Lifting from EvalT to Unify.
forceTy :: VTy -> Unify VTy
forceTy va = E.forceTy va <$> lift getMetas

($$$) :: TClosure -> VTy -> Unify VTy
cl $$$ va = cl E.$$$ va <$> lift getMetas

-- | Invert a spine, viewed as a renaming.
-- This operation can fail if the spine does not consist of
-- distinct bound variables.
invert :: Lvl -> VTSpine -> Unify PartialRenaming
invert n sp = (\(dom, ren) -> PartialRenaming dom n ren) <$> go sp
  where
    go :: VTSpine -> Unify (Lvl, HashMap Lvl Lvl)
    go = \case
      []        -> pure (0, M.empty)
      sp :> va  -> do
        (dom, ren) <- go sp
        forceTy va >>= \case
          VTyVar l | not (M.member l ren) -> pure (dom + 1, M.insert l dom ren)
          _                               -> throwError InvalidSpine

-- | Perform a partial renaming, failing if either the renaming
-- is not defined on a variable or the given metavariable occurs
-- in the value.
rename :: Metavar -> PartialRenaming -> VTy -> Unify Ty
rename m pr@(PartialRenaming dom cod ren) va = forceTy va >>= \case
  VTyVar l      -> case M.lookup l ren of
    Just l' -> pure $ TyVar (lvl2Ix dom l')
    Nothing -> throwError $ EscapingVariable (lvl2Ix cod l)
  VData l sp    -> Data l <$> renameSp m pr sp
  VMeta m' sp   | m == m'   -> throwError $ Occurs m
                | otherwise -> Meta m' <$> renameSp m pr sp
  VTInt         -> pure Int
  VTFloat       -> pure Float
  VTChar        -> pure Char
  VTTuple vas   -> Tuple <$> (rename m pr <$$> vas)
  VWorld va     -> World <$> rename m pr va
  VFun va vb    -> Fun <$> rename m pr va <*> rename m pr vb
  VForAll x cl  -> ForAll x <$> (rename m (liftPR pr) =<< cl $$$ VTyVar cod)

renameSp :: Metavar -> PartialRenaming -> VTSpine -> Unify TSpine
renameSp m pr = \case
  []        -> pure []
  sp :> va  -> (:>) <$> renameSp m pr sp <*> rename m pr va

-- | Solve a pattern unification problem.
solve :: Lvl -> Metavar -> VTSpine -> VTy -> Unify ()
solve n m sp va = do
  pr <- invert n sp
  sol <- rename m pr va
  lift $ updateMeta m sol

-- | Unify expected and actual types.
unify' :: Lvl -> VTy -> VTy -> Unify ()
unify' n va vb = forceTy va >>= \va -> forceTy vb >>= \vb -> case (va, vb) of
  (VTyVar l    , VTyVar l'     )  | l == l'  -> pure ()
  (VData l sp  , VData l' sp'  )  | l == l'  -> unifySp n sp sp'
  (VMeta m sp  , VMeta m' sp'  )  | m == m'  -> unifySp n sp sp'
  (VTInt       , VTInt         )  -> pure ()
  (VTFloat     , VTFloat       )  -> pure ()
  (VTChar      , VTChar        )  -> pure ()
  (VTTuple vas , VTTuple vbs   )  | length vas == length vbs -> unifyTup n vas vbs
  (VWorld va   , VWorld vb     )  -> unify' n va vb
  (VFun va vb  , VFun va' vb'  )  -> unify' n va va' >> unify' n vb vb'
  (VForAll x cl, VForAll x' cl')  -> bindM2 (unify' (n + 1)) (cl $$$ VTyVar n) (cl' $$$ VTyVar n)
  (VMeta m sp  , VMeta m' sp'  )  -> solve n m sp vb `catchError` \_ -> solve n m' sp' va
  (VMeta m sp  , vb            )  -> solve n m sp vb
  (va          , VMeta m sp    )  -> solve n m sp va
  _                               -> throwError RigidMismatch

unifySp :: Lvl -> VTSpine -> VTSpine -> Unify ()
unifySp n sp sp' = case (sp, sp') of
  ([]      , []        )  -> pure ()
  (sp :> va, sp' :> va')  -> unifySp n sp sp' >> unify' n va va'
  _                       -> throwError RigidMismatch

unifyTup :: Lvl -> [VTy] -> [VTy] -> Unify ()
unifyTup n vas vbs = case (vas, vbs) of
  ([]      , []      )  -> pure ()
  (va : vas, vb : vbs)  -> unify' n va vb >> unifyTup n vas vbs
  _                     -> devError "bad size for tuple"

-- | Unify expected and actual types, given the size of
-- the current type context.
--
-- @since 1.0.0
unify :: MonadMeta m => Lvl -> VTy -> VTy -> m (Maybe UnificationErrorReason)
unify n va vb = either Just (const Nothing) <$> runExceptT (unify' n va vb)
