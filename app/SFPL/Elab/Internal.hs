{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards #-}

module SFPL.Elab.Internal where

import Control.Arrow
import Control.Monad
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import SFPL.Base
import SFPL.Elab.Class
import SFPL.Elab.Context
import SFPL.Elab.Error.Types
import SFPL.Elab.Metacontext
import qualified SFPL.Elab.Unification as U
import qualified SFPL.Eval as E
import SFPL.Eval.Types
import SFPL.Syntax.Core.Types
import SFPL.Syntax.Raw.Types (Raw (..))
import qualified SFPL.Syntax.Raw.Types as R
import SFPL.Utils

infixl 9 $$$

----------------------------------------
-- Helpers

type M a = forall m. MonadElab m => m a

-- Lookup

lookupTy :: TyName -> M (Maybe TyEntry)
lookupTy x = M.lookup x . types . names <$> getElabCxt

lookupTm :: Name -> M (Maybe TmEntry)
lookupTm x = M.lookup x . terms . names <$> getElabCxt

getTyEnv :: M TEnv
getTyEnv = tyEnv <$> getElabCxt

getTyLvl :: M Lvl
getTyLvl = tyLvl <$> getElabCxt

getTmLvl :: M Lvl
getTmLvl = tmLvl <$> getElabCxt

-- Errors

registerError :: Raw r => r -> ElabErrorType -> [ElabErrorItem] -> M ()
registerError r errorType errorItems = do
  cxt <- getElabCxt
  registerElabError $ ElabError cxt (spanOf r) errorType errorItems

stopAfter :: MonadElab m => m a -> m b
stopAfter ma = ma >> throwElabErrors

notInScope :: Raw r => r -> Name -> [SyntacticCategory] -> M ()
notInScope r x cats = registerError r (NotInScopeError x cats) []

badTyApp :: R.Ty -> M ()
badTyApp r = registerError r (MalformedTypeError r IllegalApplication) []

badData :: R.Ty -> TyName -> Int -> Int -> M ()
badData r x n n' =
  registerError r (MalformedTypeError r $ BadDataApplication x n n') []

badConstructor :: R.Pattern -> M ()
badConstructor r = registerError r (MalformedPatternError r) []

cantUnify :: Raw r => r -> VTy -> VTy -> UnificationErrorReason -> M ()
cantUnify r vexp vact reason =
  registerError r (UnificationError vexp vact reason) []

-- Managing type context

-- | Create a fresh metavariable with the given name.
freshNamedMeta :: TyName -> M Ty
freshNamedMeta x = do
  n <- getTyLvl
  x <- freshName x
  freshMeta $ MetaInfo n x

-- | Bind a new type variable.
withTyVar :: MonadElab m => TyName -> m a -> m a
withTyVar x = withElabCxt (bindTyVar x)
  where
    bindTyVar x ElabCxt {..} =
      let Namespaces {..} = names
          PrintCxt {..} = printInfo
          types' = M.insert x (TyVarEntry tyLvl) types
          tyVars' = tyVars :> x
          tyEnv' = tyEnv :> VTyVar tyLvl
          tyLvl' = tyLvl + 1
      in
      let names = Namespaces {types = types', ..}
          printInfo = PrintCxt {tyVars = tyVars', ..}
      in ElabCxt {tyEnv = tyEnv', tyLvl = tyLvl', ..}

-- | Bind a new implicit type variable.
withImplTyVar :: MonadElab m => TyName -> m a -> m a
withImplTyVar x = withElabCxt (bindImplTyVar x)
  where
    bindImplTyVar x ElabCxt {..} =
      let PrintCxt {..} = printInfo
          tyVars' = tyVars :> x
          tyEnv' = tyEnv :> VTyVar tyLvl
          tyLvl' = tyLvl + 1
      in
      let printInfo = PrintCxt {tyVars = tyVars', ..}
      in ElabCxt {tyEnv = tyEnv', tyLvl = tyLvl', ..}

--  Type evaluation
-- Lifting from EvalT to MonadElab

($$$) :: TClosure -> VTy -> M VTy
cl $$$ va = cl E.$$$ va <$> getMetas

enterClosure :: TClosure -> M VTy
enterClosure cl = do
  n <- getTyLvl
  cl $$$ VTyVar n

evalTy :: TEnv -> Ty -> M VTy
evalTy env a = E.evalTy env a <$> getMetas

evalTy' :: Ty -> M VTy
evalTy' a = getTyEnv >>= \env -> evalTy env a

forceTy :: VTy -> M VTy
forceTy va = E.forceTy va <$> getMetas

-- Unification

unify :: Raw r => r -> VTy -> VTy -> M ()
unify r vexp vact = do
  n <- getTyLvl
  res <- U.unify n vexp vact
  case res of
    Nothing     -> pure ()
    Just reason -> cantUnify r vexp vact reason

------------------------------------------------------------
-- Types

checkTyIden :: R.Ty -> TyName -> M Ty
checkTyIden r x = do
  me <- lookupTy x
  case me of
    Nothing     -> stopAfter $ notInScope r x [SCType, SCVariable]
    Just entry  -> case entry of
      DataEntry l c _ | c == 0    -> pure $ Data l []
                      | otherwise -> stopAfter $ badData r x c 0
      TyVarEntry l    -> (\n -> TyVar (lvl2Ix n l)) <$> getTyLvl

checkTuple :: [R.Ty] -> M Ty
checkTuple = \case
  [a] -> checkTy a
  as  -> Tuple <$> (checkTy <$$> as)

checkTApp :: R.Ty -> R.Ty -> R.TSpine -> M Ty
checkTApp r a as = case a of
  r'@(R.TyIden x _) -> do
    me <- lookupTy x
    case me of
      Nothing     -> stopAfter $ notInScope r' x [SCType]
      Just entry  -> case entry of
        DataEntry l c _ | c == spLen -> Data l <$> (checkTy <$$> as)
                        | otherwise  -> stopAfter $ badData r x c spLen
        TyVarEntry _    -> illegal
  _                 -> illegal
  where
    illegal = stopAfter $ badTyApp r
    spLen = length as

checkForAll :: [TyName] -> R.Ty -> M Ty
checkForAll xs a = case xs of
  []      -> checkTy a
  x : xs  -> ForAll x <$> (withTyVar x $ checkForAll xs a)

-- | Check that a type is well-formed.
checkTy :: R.Ty -> M Ty
checkTy r = case r of
  R.TyIden x _    -> checkTyIden r x
  R.THole _       -> freshNamedMeta "t"
  R.Int _ _       -> pure Int
  R.Float _ _     -> pure Float
  R.Char _ _      -> pure Char
  R.Tuple as _ _  -> checkTuple as
  R.World a _     -> World <$> checkTy a
  R.TApp a as     -> checkTApp r a as
  R.Fun a b       -> Fun <$> checkTy a <*> checkTy b
  R.ForAll xs a _ -> checkForAll xs a

------------------------------------------------------------
-- Patterns

-- | The bindings that a pattern establishes.
-- A binding is either a term or type variable binding.
-- For terms, their type is also stored.
type PatternBindings = [Either (Name, VTy) TyName]

-- | Check that a pattern has the given type.
-- Returns the list of variables the pattern binds.
checkPat :: R.Pattern -> VTy -> M (Pattern, PatternBindings)
checkPat r vexp = case r of
  R.WildcardPat _ -> pure (PWildcard, [])
  _               -> do
    (pat, vact, bindings) <- inferPat r
    unify r vexp vact
    pure (pat, bindings)

inferTuplePat :: [Name] -> M (Pattern, VTy, PatternBindings)
inferTuplePat xs = do
  (vas, bindings) <- go xs tupleMetaNames
  pure (PTuple xs, VTTuple vas, bindings)
  where
    go xs ms = case (xs, ms) of
      ([]    , _     )  -> pure ([], [])
      (x : xs, m : ms)  -> do
        va <- evalTy' =<< freshNamedMeta m
        (vas, bindings) <- go xs ms
        pure (va : vas, Left (x, va) : bindings)
    tupleMetaNames = [replicate c x | c <- [1 ..], x <- ['a' .. 'z']]

insertForCtr :: Ty -> [TyName] -> M VTy
insertForCtr a xs = createMetas [] xs >>= \ms -> evalTy ms a
  where
    createMetas ms = \case
      []      -> pure ms
      x : xs  -> do
        m <- evalTy' =<< freshNamedMeta x
        createMetas (ms :> m) xs

inferCtrPat :: R.Pattern -> Name -> R.CtrArgs -> M (Pattern, VTy, PatternBindings)
inferCtrPat r x args = do
  me <- lookupTm x
  case me of
    Nothing     -> scopeError
    Just entry  -> case entry of
      ConstructorEntry l a xs _ -> do
        va <- insertForCtr a xs
        (va, bindings) <- go args va
        let args = map (fst +++ id) bindings
        pure (PCtr l args, va, bindings)
      _                         -> scopeError
  where
    scopeError = stopAfter $ notInScope r x [SCConstructor]
    go args va = case args of
      []          -> pure (va, [])
      arg : args' -> forceTy va >>= \va -> case (arg, va) of
        (Left x , VFun va vb  ) -> do
          (va', bindings) <- go args' vb
          pure (va', Left (x, va) : bindings)
        (Left x , VForAll y cl) -> do
          vb <- enterClosure cl
          withImplTyVar y $ do
            (va', bindings) <- go args vb
            pure (va', Right y : bindings)
        (Right x, VForAll y cl) -> do
          vb <- enterClosure cl
          withTyVar x $ do
            (va', bindings) <- go args' vb
            pure (va', Right x : bindings)
        _                       -> stopAfter $ badConstructor r

-- | Infer the type of a pattern.
-- Returns the inferred type and the list of variables
-- the pattern binds.
inferPat :: R.Pattern -> M (Pattern, VTy, PatternBindings)
inferPat r = case r of
  R.IntPat n _ _      -> pure (PInt n, VTInt, [])
  R.FloatPat n _ _    -> pure (PFloat n, VTFloat, [])
  R.CharPat c _ _     -> pure (PChar c, VTChar, [])
  R.TuplePat xs _ _   -> inferTuplePat xs
  R.EmptyListPat _ _  -> inferCtrPat r dsNil []
  R.ConsPat x y _ _   -> inferCtrPat r dsCons [Left x, Left y]
  R.CtrPat x args _ _ -> inferCtrPat r x args
  R.WildcardPat _     -> devError "shouldn't infer wildcard pattern"

------------------------------------------------------------
-- Terms



------------------------------------------------------------
-- Type declarations


