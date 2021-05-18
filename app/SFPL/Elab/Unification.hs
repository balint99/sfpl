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
type U a = forall m. MonadMeta m => ExceptT UnificationErrorReason m a

-- Lifting from MonadMeta to U.
forceTy :: VTy -> U VTy
forceTy va = lift $ E.forceTy va

($$$) :: TClosure -> VTy -> U VTy
cl $$$ va = lift $ cl E.$$$ va

-- | Invert a spine, viewed as a renaming.
-- This operation can fail if the spine does not consist of
-- distinct bound variables.
invert :: Lvl -> VTSpine -> U PartialRenaming
invert n sp = (\(dom, ren) -> PartialRenaming dom n ren) <$> go sp
  where
    go :: VTSpine -> U (Lvl, HashMap Lvl Lvl)
    go = \case
      []        -> pure (0, M.empty)
      sp :> va  -> do
        (dom, ren) <- go sp
        case va of
          VTyVar l | not (M.member l ren) -> pure (dom + 1, M.insert l dom ren)
          _                               -> throwError InvalidSpine

-- | Perform a partial renaming, failing if either the renaming
-- is not defined on a variable or the given metavariable occurs
-- in the value.
rename :: Metavar -> PartialRenaming -> VTy -> U Ty
rename m pr@(PartialRenaming dom cod ren) va = forceTy va >>= \case
  VTyVar l      -> case M.lookup l ren of
    Just l' -> pure $ TyVar (lvl2Ix dom l')
    Nothing -> throwError $ EscapingVariable l
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

renameSp :: Metavar -> PartialRenaming -> VTSpine -> U TSpine
renameSp m pr = \case
  []        -> pure []
  sp :> va  -> (:>) <$> renameSp m pr sp <*> rename m pr va

-- | Solve a pattern unification problem.
solve :: Lvl -> Metavar -> VTSpine -> VTy -> U ()
solve n m sp va = do
  pr <- invert n sp
  sol <- rename m pr va
  lift $ updateMeta m sol

-- | Unify expected and actual types.
unify :: Lvl -> VTy -> VTy -> U ()
unify n va vb = pair (forceTy va) (forceTy vb) >>= \case
  (VTyVar l    , VTyVar l'     )  | l == l'  -> pure ()
  (VData l sp  , VData l' sp'  )  | l == l'  -> unifySp n sp sp'
  (VMeta m sp  , VMeta m' sp'  )  | m == m'  -> unifySp n sp sp'
  (VTInt       , VTInt         )  -> pure ()
  (VTFloat     , VTFloat       )  -> pure ()
  (VTChar      , VTChar        )  -> pure ()
  (VTTuple vas , VTTuple vbs   )  -> unifyList n vas vbs
  (VWorld va   , VWorld vb     )  -> unify n va vb
  (VFun va vb  , VFun va' vb'  )  -> unify n va va' >> unify n vb vb'
  (VForAll x cl, VForAll x' cl')  -> bindM2 (unify (n + 1)) (cl $$$ VTyVar n) (cl' $$$ VTyVar n)
  (VMeta m sp  , va            )  -> solve n m sp va
  (va          , VMeta m sp    )  -> solve n m sp va
  _                               -> throwError RigidMismatch

unifySp :: Lvl -> VTSpine -> VTSpine -> U ()
unifySp n sp sp' = case (sp, sp') of
  ([]      , []       ) -> pure ()
  (sp :> va, sp' :> vb) -> unifySp n sp sp' >> unify n va vb
  _                     -> throwError RigidMismatch

unifyList :: Lvl -> [VTy] -> [VTy] -> U ()
unifyList n vas vbs = case (vas, vbs) of
  ([]      , []      )  -> pure ()
  (va : vas, vb : vbs)  -> unify n va vb >> unifyList n vas vbs
  _                     -> throwError RigidMismatch
