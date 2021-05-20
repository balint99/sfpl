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
import SFPL.Syntax.Raw.Types (Raw (..), BegPos)
import qualified SFPL.Syntax.Raw.Types as R
import SFPL.Utils

infixl 9 $$$

----------------------------------------
-- Helpers

type M a = forall m. MonadElab m => m a

-- Lookup

getNextTypeDecl :: M Lvl
getNextTypeDecl = nextTypeDecl . topLevelCxt <$> getElabCxt

getNextConstructor :: M Lvl
getNextConstructor = nextConstructor . topLevelCxt <$> getElabCxt

getNextTopLevelDef :: M Lvl
getNextTopLevelDef = nextTopLevelDef . topLevelCxt <$> getElabCxt

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

multipleDecls :: Raw r => r -> Name -> BegPos -> M ()
multipleDecls r x beg = registerError r (MultipleDeclarationsError x beg) []

badTyApp :: R.Ty -> M ()
badTyApp r = registerError r (MalformedTypeError r IllegalApplication) []

badData :: R.Ty -> TyName -> Int -> Int -> M ()
badData r x n n' =
  registerError r (MalformedTypeError r $ BadDataApplication x n n') []

invalidHole :: R.Ty -> TypeHolePlace -> M ()
invalidHole r place = registerError r (InvalidTypeHoleError r place) []

badCtrReturnType :: R.Ty -> Name -> M ()
badCtrReturnType r x = registerError r (BadConstructorType x r) []

badCtrPat :: R.Pattern -> M ()
badCtrPat r = registerError r (MalformedPatternError r) []

cantUnify :: Raw r => r -> VTy -> VTy -> UnificationErrorReason -> M ()
cantUnify r vexp vact reason =
  registerError r (UnificationError vexp vact reason) []

newHole :: R.Tm -> VTy -> M ()
newHole r va = registerError r (HoleError va) [Bindings]

-- Managing context

-- | Bind a new type variable.
withTyVar :: MonadElab m => TyName -> m a -> m a
withTyVar x = withElabCxt bindTyVar
  where
    bindTyVar ElabCxt {..} =
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

-- | Bind multiple type variables.
withTyVars :: MonadElab m => [TyName] -> m a -> m a
withTyVars = foldr (\x f -> withTyVar x . f) id

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

-- | Reset all local information, but keep top-level bindings.
atTopLevel :: MonadElab m => m a -> m a
atTopLevel = withElabCxt dropLocal
  where
    dropLocal ElabCxt {..} =
      let PrintCxt {..} = printInfo
      in
      let printInfo = PrintCxt {tyVars = [], tmVars = [], ..}
      in ElabCxt {tyEnv = [], tyLvl = 0, tmLvl = 0, ..}

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

-- | Create a fresh metavariable with the given name.
freshNamedMeta :: TyName -> M Ty
freshNamedMeta x = do
  n <- getTyLvl
  x <- freshName x
  freshMeta $ MetaInfo n x

-- | Create a fresh evaluated metavariable with the given name.
freshNamedMeta' :: TyName -> M VTy
freshNamedMeta' = evalTy' <=< freshNamedMeta

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

checkTuple :: MonadElab m => m Ty -> [R.Ty] -> m Ty
checkTuple h = \case
  [a] -> checkTy' h a
  as  -> Tuple <$> (checkTy' h <$$> as)

checkTApp' :: MonadElab m => m Ty -> R.Ty -> R.Ty -> Name -> R.TSpine -> m Ty
checkTApp' h r r' x as = do
  me <- lookupTy x
  case me of
    Nothing     -> stopAfter $ notInScope r' x [SCType]
    Just entry  -> case entry of
      DataEntry l c _ | c == spLen -> Data l <$> (checkTy' h <$$> reverse as)
                      | otherwise  -> stopAfter $ badData r x c spLen
      TyVarEntry _    -> illegal
  where
    illegal = stopAfter $ badTyApp r
    spLen = length as
    

checkTApp :: MonadElab m => m Ty -> R.Ty -> R.Ty -> R.TSpine -> m Ty
checkTApp h r a as = case a of
  r'@(R.TyIden x _) -> checkTApp' h r r' x as
  _                 -> illegal
  where
    illegal = stopAfter $ badTyApp r

checkForAll :: MonadElab m => m Ty -> [TyName] -> R.Ty -> m Ty
checkForAll h xs a = case xs of
  []      -> checkTy' h a
  x : xs  -> ForAll x <$> (withTyVar x $ checkForAll h xs a)

-- | Check that a type is well-formed. The first parameter gives
-- the action to perform on holes.
checkTy' :: MonadElab m => m Ty -> R.Ty -> m Ty
checkTy' h r = case r of
  R.TyIden x _    -> checkTyIden r x
  R.THole _       -> h
  R.Int _ _       -> pure Int
  R.Float _ _     -> pure Float
  R.Char _ _      -> pure Char
  R.Tuple as _ _  -> checkTuple h as
  R.List a _ _    -> checkTApp' h r r dsList [a]
  R.World a _     -> World <$> checkTy' h a
  R.TApp a as     -> checkTApp h r a as
  R.Fun a b       -> Fun <$> checkTy' h a <*> checkTy' h b
  R.ForAll xs a _ -> checkForAll h xs a

-- | Check that a type is well-formed. Creates fresh metavariables on holes.
checkTy :: R.Ty -> M Ty
checkTy = checkTy' (freshNamedMeta "t")

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
        va <- freshNamedMeta' m
        (vas, bindings) <- go xs ms
        pure (va : vas, Left (x, va) : bindings)
    tupleMetaNames = [replicate c x | c <- [1 ..], x <- ['a' .. 'z']]

insertForCtr :: VTy -> Int -> M VTy
insertForCtr va = \case
  0 -> pure va
  -- no need to force here, because a constructors type
  -- cannot contain metavariables
  c -> case va of
    VForAll x cl  -> do
      m <- freshNamedMeta' x
      vb <- cl $$$ m
      insertForCtr vb (c - 1)
    _             -> devError "can't insert for constructor: not a forall type"

inferCtrPat :: R.Pattern -> Name -> R.CtrArgs -> M (Pattern, VTy, PatternBindings)
inferCtrPat r x args = do
  me <- lookupTm x
  case me of
    Nothing     -> scopeError
    Just entry  -> case entry of
      ConstructorEntry l va c _ -> do
        va <- insertForCtr va c
        (va, bindings) <- go args va
        let args = map (fst +++ id) bindings
        pure (PCtr l args, va, bindings)
      _                         -> scopeError
  where
    scopeError = stopAfter $ notInScope r x [SCConstructor]
    go args va = case args of
      []          -> pure (va, [])
      -- no need to force va, because it only contains
      -- fresh metavariables
      arg : args' -> case (arg, va) of
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
        _                       -> stopAfter $ badCtrPat r

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

-- | Check that a term has the given type.
checkTm :: R.Tm -> VTy -> M Tm
checkTm r va = forceTy va >>= \va -> case (r, va) of
  _ -> undefined

-- | Infer the type of a term.
inferTm :: R.Tm -> M (Tm, VTy)
inferTm r = case r of
  _ -> undefined

----------------------------------------
-- Top-level definitions

checkTopLevelDefScope :: R.TopLevelDef -> M ()
checkTopLevelDefScope r@(R.TL x _ _ _ _) = do
  me <- lookupTm x
  case me of
    Nothing     -> pure ()
    Just entry  -> case entry of
      TopLevelEntry _ _ prev      -> err prev
      ConstructorEntry _ _ _ prev -> err prev
      _                           -> devError "previous declaration was not a top-level binding"
  where
    err prev = stopAfter $ multipleDecls r x prev

-- | Bind a new top-level definition.
withTopLevel :: MonadElab m => Name -> VTy -> BegPos -> m a -> m a
withTopLevel x va beg = withElabCxt bindTopLevel
  where
    bindTopLevel ElabCxt {..} =
      let TopLevelCxt {..} = topLevelCxt
          Namespaces {..} = names
          PrintCxt {..} = printInfo
          terms' = M.insert x (TopLevelEntry nextTopLevelDef va beg) terms
          nextTopLevelDef' = nextTopLevelDef + 1
          topLevelDefNames' = topLevelDefNames :> x
      in
      let topLevelCxt = TopLevelCxt {nextTopLevelDef = nextTopLevelDef', ..}
          names = Namespaces {terms = terms', ..}
          printInfo = PrintCxt {topLevelDefNames = topLevelDefNames', ..}
      in ElabCxt {..}

-- | Elaborate a top-level definition.
withTopLevelDef :: MonadElab m => R.TopLevelDef -> (TopLevelDef -> m a) -> m a
withTopLevelDef r@(R.TL x a t beg _) f = do
  checkTopLevelDefScope r
  l <- getNextTopLevelDef
  a <- checkTy' illegalTy a
  va <- evalTy [] a
  withTopLevel x va beg $ do
    t <- checkTm t va
    let tl = TL x a t
    f tl
  where
    illegalTy = stopAfter $ invalidHole a THPTopLevelDef

------------------------------------------------------------
-- Type declarations

----------------------------------------
-- Constructors

returnType :: R.Ty -> R.Ty
returnType = \case
  R.Fun a b       -> returnType b
  R.ForAll x a _  -> returnType a
  r               -> r

checkReturnType :: Name -> Lvl -> R.Ty -> Ty -> Ix -> M ()
checkReturnType x l r a n = case a of
  Fun a b     -> checkReturnType x l r b n
  ForAll y a  -> checkReturnType x l r a (n + 1)
  Data l' sp  | l == l'   -> go sp n
              | otherwise -> illegal
  _           -> illegal
  where
    illegal = stopAfter $ badCtrReturnType r x
    go sp n = case sp of
      []            -> pure ()
      sp :> TyVar i | i == n  -> go sp (n + 1)
      _             -> illegal

-- | Check that a data constructor's type is well-formed
-- and that it is appropriate: doesn't contain any holes,
-- and the return type is the same as the data type that defines it.
checkCtrTy :: Name -> Lvl -> R.Ty -> M Ty
checkCtrTy x l r = do
  let ret = returnType r
  a <- checkTy' illegalTy r
  checkReturnType x l ret a 0
  pure a
  where
    illegalTy = stopAfter $ invalidHole r THPConstructor

-- | Elaborate a constructor declaration.
checkCtr :: Lvl -> R.Constructor -> M (Constructor, Name, BegPos)
checkCtr parentLvl r@(R.Constructor x a beg) = do
  me <- lookupTm x
  case me of
    Nothing -> do
      l <- getNextConstructor
      a <- checkCtrTy x parentLvl a
      pure (Constructor l a, x, beg)
    Just entry  -> case entry of
      TopLevelEntry _ _ prev      -> err prev
      ConstructorEntry _ _ _ prev -> err prev
      _                           -> devError "previous declaration was not a top-level binding"
  where
    err prev = stopAfter $ multipleDecls r x prev

----------------------------------------
-- Data type declarations

checkDataDeclScope :: R.DataDecl -> M ()
checkDataDeclScope r@(R.DD x _ _ _ _) = do
  me <- lookupTy x
  case me of
    Nothing     -> pure ()
    Just entry  -> case entry of
      DataEntry _ _ prev  -> stopAfter $ multipleDecls r x prev
      _                   -> devError "previous declaration was not a top-level binding"

-- | Bind a new data type.
withDataType :: MonadElab m => TyName -> Int -> BegPos -> m a -> m a
withDataType x c beg = withElabCxt bindDataType
  where
    bindDataType ElabCxt {..} =
      let TopLevelCxt {..} = topLevelCxt
          Namespaces {..} = names
          PrintCxt {..} = printInfo
          types' = M.insert x (DataEntry nextTypeDecl c beg) types
          nextTypeDecl' = nextTypeDecl + 1
          typeNames' = typeNames :> x
      in
      let topLevelCxt = TopLevelCxt {nextTypeDecl = nextTypeDecl', ..}
          names = Namespaces {types = types', ..}
          printInfo = PrintCxt {typeNames = typeNames', ..}
      in ElabCxt {..}

-- | Bind a new data constructor.
withConstructor :: MonadElab m => Name -> VTy -> Int -> BegPos -> m a -> m a
withConstructor x va c beg = withElabCxt bindConstructor
  where
    bindConstructor ElabCxt {..} =
      let TopLevelCxt {..} = topLevelCxt
          Namespaces {..} = names
          PrintCxt {..} = printInfo
          terms' = M.insert x (ConstructorEntry nextConstructor va c beg) terms
          nextConstructor' = nextConstructor + 1
          constructorNames' = constructorNames :> x
      in
      let topLevelCxt = TopLevelCxt {nextConstructor = nextConstructor', ..}
          names = Namespaces {terms = terms', ..}
          printInfo = PrintCxt {constructorNames = constructorNames', ..}
      in ElabCxt {..}

-- | Add universal quantifiers to a constructors type, which binds
-- the type parameters of its parent data type.
addForAlls :: Ty -> M Ty
addForAlls a = do
  xs <- tyVars . printInfo <$> getElabCxt
  pure $ foldl (\a x -> ForAll x a) a xs

-- | Elaborate a data type declaration, creating new bindings
-- for the type and its constructors.
withDataDecl :: MonadElab m => R.DataDecl -> (DataDecl -> m a) -> m a
withDataDecl r@(R.DD x xs cs beg _) f = do
  checkDataDeclScope r
  l <- getNextTypeDecl
  let c = length xs
  withDataType x c beg $
    withTyVars xs $
      goConstructors l c [] cs
  where
    goConstructors l c cs = \case
      []        -> do
        let dd = DD l xs (reverse cs)
        atTopLevel $ f dd
      r : rs  -> do
        (ctr, x, beg) <- checkCtr l r
        let Constructor _ a = ctr
        a <- addForAlls a
        va <- evalTy [] a
        withConstructor x va c beg $
          goConstructors l c (ctr : cs) rs

----------------------------------------

-- | Elaborate a type declaration.
withTypeDecl :: MonadElab m => R.TypeDecl -> (TypeDecl -> m a) -> m a
withTypeDecl r f = case r of
  R.DataDecl dd -> withDataDecl dd (f . DataDecl)

------------------------------------------------------------
-- Programs

-- | Elaborate a program.
