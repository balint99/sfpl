{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}

module SFPL.Elab.Internal where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Maybe
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
import SFPL.Syntax.Raw.Types (Raw (..), BegPos, EndPos)
import qualified SFPL.Syntax.Raw.Types as R
import SFPL.Utils

infixl 9 $$$

----------------------------------------
-- Helpers

type M a = forall m. MonadElab m => m a
type MM a = forall m. MonadElab m => MaybeT m a

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

checkForErrors :: MonadElab m => m a -> m a
checkForErrors m = do
  a <- m
  b <- isErrorRegistered
  if b
    then throwElabErrors
    else pure a

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

badImplApp :: R.Tm -> VTy -> R.Ty -> M ()
badImplApp r va b = registerError r (ImplicitApplicationError va b) []

typeNotSupported :: R.Tm -> OverloadType -> VTy -> M ()
typeNotSupported r ot va = registerError r (TypeNotSupportedError ot va) []

ambiguousOverloading :: R.Tm -> OverloadType -> M ()
ambiguousOverloading r ot = registerError r (AmbiguousOverloadingError ot) []

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

-- | Bind a new variable.
withVar :: MonadElab m => Name -> VTy -> m a -> m a
withVar x va = withElabCxt bindVar
  where
    bindVar ElabCxt {..} =
      let Namespaces {..} = names
          PrintCxt {..} = printInfo
          terms' = M.insert x (VarEntry tmLvl va) terms
          tmVars' = tmVars :> x
          tmLvl' = tmLvl + 1
      in
      let names = Namespaces {terms = terms', ..}
          printInfo = PrintCxt {tmVars = tmVars', ..}
      in ElabCxt {tmLvl = tmLvl', ..}

-- | Bind the bindings induced by a pattern.
withPatternBindings :: MonadElab m => PatternBindings -> m a -> m a
withPatternBindings = foldr (\x f -> step x . f) id
  where
    step = either (uncurry withVar) (either withTyVar withImplTyVar)

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
enterClosure cl = getTyLvl >>= \n -> cl $$$ VTyVar n

evalTy :: TEnv -> Ty -> M VTy
evalTy env a = E.evalTy env a <$> getMetas

evalTy' :: Ty -> M VTy
evalTy' a = getTyEnv >>= \env -> evalTy env a

forceTy :: VTy -> M VTy
forceTy va = E.forceTy va <$> getMetas

quoteTy :: Lvl -> VTy -> M Ty
quoteTy n va = E.quoteTy n va <$> getMetas

quoteTy' :: VTy -> M Ty
quoteTy' va = getTyLvl >>= \n -> quoteTy n va

-- Close a type value
closeTy :: VTy -> M TClosure
closeTy va = do
  env <- getTyEnv
  n <- getTyLvl
  TClosure env <$> quoteTy (n + 1) va

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

unify :: Raw r => r -> VTy -> VTy -> M Bool
unify r vexp vact = do
  n <- getTyLvl
  res <- U.unify n vexp vact
  case res of
    Nothing     -> pure True
    Just reason -> False <$ cantUnify r vexp vact reason

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

checkTApp :: MonadElab m => m Ty -> R.Ty -> R.Ty -> R.TSpine -> m Ty
checkTApp h r a as = case a of
  r'@(R.TyIden x _ _) -> do
    me <- lookupTy x
    case me of
      Nothing     -> stopAfter $ notInScope r' x [SCType]
      Just entry  -> case entry of
        DataEntry l c _ | c == spLen -> Data l <$> (checkTy' h <$$> reverse as)
                        | otherwise  -> stopAfter $ badData r x c spLen
        TyVarEntry _    -> illegal
  R.List a beg end    -> checkTApp h r (R.TyIden dsList beg end) (a : as)
  _                   -> illegal
  where
    illegal = stopAfter $ badTyApp r
    spLen = length as

checkForAll :: MonadElab m => m Ty -> [TyName] -> R.Ty -> m Ty
checkForAll h xs a = case xs of
  []      -> checkTy' h a
  x : xs  -> ForAll x <$> (withTyVar x $ checkForAll h xs a)

-- | Check that a type is well-formed. The first parameter gives
-- the action to perform on holes.
checkTy' :: MonadElab m => m Ty -> R.Ty -> m Ty
checkTy' h r = case r of
  R.TyIden x _ _    -> checkTyIden r x
  R.THole _         -> h
  R.Int _ _         -> pure Int
  R.Float _ _       -> pure Float
  R.Char _ _        -> pure Char
  R.Tuple as _ _    -> checkTuple h as
  R.List a beg end  -> checkTApp h r (R.TyIden dsList beg end) [a]
  R.World a _       -> World <$> checkTy' h a
  R.TApp a as       -> checkTApp h r a as
  R.Fun a b         -> Fun <$> checkTy' h a <*> checkTy' h b
  R.ForAll xs a _   -> checkForAll h xs a

-- | Check that a type is well-formed. Creates fresh metavariables on holes.
checkTy :: R.Ty -> M Ty
checkTy = checkTy' (freshNamedMeta "t")

------------------------------------------------------------
-- Patterns

-- | The bindings that a pattern establishes.
-- A binding is either a term or type variable binding.
-- For terms, their type is also stored.
-- For types, the binding is either explicit or implicit.
type PatternBindings = [Either (Name, VTy) (Either TyName TyName)]

-- | Check that a pattern has the given type.
-- Returns the list of variables the pattern binds in a 'Just',
-- or 'Nothing' if checking fails.
checkPat :: R.Pattern -> VTy -> M (Maybe (Pattern, PatternBindings))
checkPat r vexp = runMaybeT $ checkPat' r vexp

checkPat' :: R.Pattern -> VTy -> MM (Pattern, PatternBindings)
checkPat' r vexp = case r of
  R.WildcardPat _ -> pure (PWildcard, [])
  _               -> do
    (pat, vact, bindings) <- inferPat' r
    success <- unify r vexp vact
    if success then pure (pat, bindings) else mzero

tupleMetaNames :: [String]
tupleMetaNames = [replicate c x | c <- [1 ..], x <- ['a' .. 'z']]

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

inferCtrPat :: R.Pattern -> Name -> R.CtrArgs -> MM (Pattern, VTy, PatternBindings)
inferCtrPat r x args = do
  me <- lookupTm x
  case me of
    Nothing     -> scopeError
    Just entry  -> case entry of
      ConstructorEntry l va c _ -> do
        va <- insertForCtr va c
        (va, bindings) <- go args va
        let args = map (fst +++ (id ||| id)) bindings
        pure (PCtr l args, va, bindings)
      _                         -> scopeError
  where
    scopeError = notInScope r x [SCConstructor] >> mzero
    go args va = case args of
      []                  -> pure (va, [])
      -- no need to force va, because it only contains
      -- fresh metavariables
      args'@(arg : args)  -> case (arg, va) of
        (Left x , VFun va vb  ) -> do
          (vb, bindings) <- go args vb
          pure (vb, Left (x, va) : bindings)
        (Left x , VForAll y cl) -> do
          vb <- enterClosure cl
          withImplTyVar y $ do
            (vb, bindings) <- go args' vb
            pure (vb, Right (Right y) : bindings)
        (Right x, VForAll y cl) -> do
          vb <- enterClosure cl
          withTyVar x $ do
            (vb, bindings) <- go args vb
            pure (vb, Right (Left x) : bindings)
        _                       -> badCtrPat r >> mzero

-- | Infer the type of a pattern.
-- Returns the inferred type and the list of variables
-- the pattern binds in a 'Just',
-- or 'Nothing' if inference fails.
inferPat :: R.Pattern -> M (Maybe (Pattern, VTy, PatternBindings))
inferPat = runMaybeT . inferPat'

inferPat' :: R.Pattern -> MM (Pattern, VTy, PatternBindings)
inferPat' r = case r of
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

hole :: M (Tm, VTy)
hole = (Hole, ) <$> freshNamedMeta' "t"

-- | Insert fresh implicit applications.
insertImpl' :: (Tm, VTy) -> M (Tm, VTy)
insertImpl' (t, va) = forceTy va >>= \case
  VForAll x cl  -> do
    a <- freshNamedMeta x
    va <- evalTy' a
    vb <- cl $$$ va
    insertImpl' (AppI t a, vb)
  _             -> pure (t, va)

-- | Insert fresh implicit applications to a term
-- which is not an implicit lambda.
insertImpl :: (Tm, VTy) -> M (Tm, VTy)
insertImpl (t, va) = case t of
  LamI{}  -> pure (t, va)
  _       -> insertImpl' (t, va)

----------------------------------------
-- Checking

checkLam :: R.Tm -> [R.LamBind] -> R.Tm -> VTy -> M Tm
checkLam r bs t va = case bs of
  []            -> checkTm t va
  bs'@(b : bs)  -> forceTy va >>= \va -> case (b, va) of
    (R.Expl x ma, VFun va vb  ) -> do
      a <- case ma of
        Nothing -> quoteTy' va
        Just ra -> do
          a <- checkTy ra
          va' <- evalTy' a
          unify ra va va'
          pure a
      Lam x a <$> (withVar x va $ checkLam r bs t vb)
    (R.Impl x   , VForAll y cl) -> do
      vb <- enterClosure cl
      LamI x <$> (withTyVar x $ checkLam r bs t vb)
    (_          , VForAll x cl) -> do
      vb <- enterClosure cl
      LamI x <$> (withImplTyVar x $ checkLam r bs' t vb)
    (_          , vexp        ) -> do
      (t, vact) <- insertImpl =<< inferLam bs' t
      success <- unify r vexp vact
      pure $ if success then t else Hole

checkLet :: [R.LocalBind] -> R.Tm -> VTy -> M Tm
checkLet bs u vb = case bs of
  []              -> checkTm u vb
  (x, ma, t) : bs -> do
    a <- maybe (freshNamedMeta "t") checkTy ma
    va <- evalTy' a
    t <- checkTm t va
    u <- withVar x va $ checkLet bs u vb
    pure $ Let x a t u

checkTup :: [R.Tm] -> [VTy] -> M Tm
checkTup ts vas = Tup <$> go ts vas
  where
    go ts vas = case (ts, vas) of
      ([]    , []      )  -> pure []
      (t : ts, va : vas)  -> (:) <$> checkTm t va <*> go ts vas
      _                   -> devError "bad size for tuple"

checkSplitScrutinee :: R.Tm -> [Name] -> M (Tm, [(Name, VTy)])
checkSplitScrutinee t xs = do
  vas <- freshNamedMeta' <$$> zipWith (flip const) xs tupleMetaNames
  t <- checkTm t (VTTuple vas)
  pure (t, zip xs vas)

checkCasePatterns :: R.Tm -> [R.CaseBranch] ->
  M (Tm, [(Pattern, PatternBindings, R.Tm)])
checkCasePatterns t bs = inferTm t >>= \(t, va) -> (t, ) <$> go va bs
  where
    go va = \case
      []          -> pure []
      (p, u) : bs -> do
        mp <- checkPat p va
        case mp of
          Nothing             -> go va bs
          Just (p, bindings)  -> ((p, bindings, u) :) <$> go va bs

checkCaseTerms :: Tm -> [(Pattern, PatternBindings, R.Tm)] -> VTy -> M Tm
checkCaseTerms t zs va = Case t <$> go zs
  where
    go = \case
      []                    -> pure []
      (p, bindings, u) : zs -> withPatternBindings bindings $ do
        u <- checkTm u va
        bs <- go zs
        pure $ (p, u) : bs

checkDo :: [R.LocalBind] -> R.Tm -> VTy -> M Tm
checkDo bs u vb = case bs of
  []              -> checkTm u vb
  (x, ma, t) : bs -> do
    a <- maybe (freshNamedMeta "t") checkTy ma
    va <- VWorld <$> evalTy' a
    t <- checkTm t va
    u <- withVar x va $ checkDo bs u vb
    pure $ Bind x a t u

-- | Check that a term has the given type.
checkTm :: R.Tm -> VTy -> M Tm
checkTm r va = forceTy va >>= \va -> case (r, va) of
  (R.Lam bs t _        , va          )  -> checkLam r bs t va
  (t                   , VForAll x cl)  -> do
    vb <- enterClosure cl
    LamI x <$> (withImplTyVar x $ checkTm t vb)
  (R.Let bs t _        , va          )  -> checkLet bs t va
  (R.TyAnn t ra        , vexp        )  -> do
    a <- checkTy ra
    va <- evalTy' a
    unify ra vexp va
    checkTm t va
  (R.Hole _            , va          )  -> Hole <$ newHole r va
  (R.Tup [t] _ _       , va          )  -> checkTm t va
  (R.Tup ts _ _        , VTTuple vas ) | length ts == length vas -> checkTup ts vas
  (R.UnOp R.Plus t _   , VTInt       )  -> checkTm t VTInt
  (R.UnOp R.Plus t _   , VTFloat     )  -> checkTm t VTFloat
  (R.UnOp R.Minus t _  , VTInt       )  -> UnOp Negate <$> checkTm t VTInt
  (R.UnOp R.Minus t _  , VTFloat     )  -> UnOp Negate <$> checkTm t VTFloat
  (R.UnOp R.Pure t _   , VWorld va   )  -> UnOp Pure <$> checkTm t va
  (R.BinOp R.Add t u   , VTInt       )  -> BinOp Add <$> checkTm t VTInt <*> checkTm u VTInt
  (R.BinOp R.Add t u   , VTFloat     )  -> BinOp Add <$> checkTm t VTFloat <*> checkTm u VTFloat
  (R.BinOp R.Sub t u   , VTInt       )  -> BinOp Sub <$> checkTm t VTInt <*> checkTm u VTInt
  (R.BinOp R.Sub t u   , VTFloat     )  -> BinOp Sub <$> checkTm t VTFloat <*> checkTm u VTFloat
  (R.BinOp R.Mul t u   , VTInt       )  -> BinOp Mul <$> checkTm t VTInt <*> checkTm u VTInt
  (R.BinOp R.Mul t u   , VTFloat     )  -> BinOp Mul <$> checkTm t VTFloat <*> checkTm u VTFloat
  (R.BinOp R.Div t u   , VTInt       )  -> BinOp Div <$> checkTm t VTInt <*> checkTm u VTInt
  (R.BinOp R.Div t u   , VTFloat     )  -> BinOp Div <$> checkTm t VTFloat <*> checkTm u VTFloat
  (R.BinOp R.Exp t u   , VTInt       )  -> BinOp Exp <$> checkTm t VTInt <*> checkTm u VTInt
  (R.BinOp R.Exp t u   , VTFloat     )  -> BinOp Exp <$> checkTm t VTFloat <*> checkTm u VTFloat
  (R.Split t xs u _    , va          )  -> do
    (t, zs) <- checkSplitScrutinee t xs
    u <- foldr (\(x, va) f -> withVar x va . f) id zs $ checkTm u va
    pure $ Case t [(PTuple xs, u)]
  (R.Case t bs _ _     , va          )  -> do
    (t, zs) <- checkCasePatterns t bs
    checkCaseTerms t zs va
  (R.Do bs t _         , va          )  -> checkDo bs t va
  (r                   , vexp        )  -> do
    (t, vact) <- insertImpl =<< inferTm r
    success <- unify r vexp vact
    pure $ if success then t else Hole

----------------------------------------
-- Inference

inferIden :: R.Tm -> Name -> M (Tm, VTy)
inferIden r x = do
  me <- lookupTm x
  case me of
    Nothing     -> notInScope r x [SCVariable, SCConstructor] >> hole
    Just entry  -> case entry of
      TopLevelEntry l va _      -> pure (TopLevel l, va)
      ConstructorEntry l va _ _ -> pure (Ctr l, va)
      VarEntry l va             -> (\n -> (Var (lvl2Ix n l), va)) <$> getTmLvl

inferLam :: [R.LamBind] -> R.Tm -> M (Tm, VTy)
inferLam bs t = case bs of
  []      -> inferTm t
  b : bs  -> case b of
    R.Expl x ma -> do
      a <- maybe (freshNamedMeta "p") checkTy ma
      va <- evalTy' a
      (t, vb) <- withVar x va $ insertImpl =<< inferLam bs t
      pure (Lam x a t, VFun va vb)
    R.Impl x    -> do
      (t, va) <- withTyVar x $ insertImpl =<< inferLam bs t
      cl <- closeTy va
      pure (LamI x t, VForAll x cl)

ensureFun :: R.Tm -> Tm -> VTy -> M (Tm, VTy, VTy)
ensureFun r t va = forceTy va >>= \case
  VFun va vb  -> pure (t, va, vb)
  ty          -> do
    va <- freshNamedMeta' "p"
    vb <- freshNamedMeta' "t"
    success <- unify r (VFun va vb) ty
    pure $ if success then (t, va, vb) else (Hole, va, vb)

inferApp :: R.Tm -> R.Tm -> R.Tm -> M (Tm, VTy)
inferApp r t u = do
  (t, va) <- insertImpl' =<< inferTm t
  (t, va, vb) <- ensureFun r t va
  u <- checkTm u va
  pure (App t u, vb)

inferAppI :: R.Tm -> R.Tm -> R.TSpine -> M (Tm, VTy)
inferAppI r t as = case as of
  []  -> devError "empty spine in implicit application"
  _   -> inferTm t >>= \(t, va) -> go t va as
  where
    go t va = \case
      []      -> pure (t, va)
      a : as  -> do
        (t, x, cl) <- do
          let tryForAll (t, ty) = do
                let x = "p"
                cl <- TClosure <$> getTyEnv <*> (withImplTyVar x $ freshNamedMeta "t")
                success <- unify r (VForAll x cl) ty
                pure $ if success then (t, x, cl) else (Hole, x, cl)
          forceTy va >>= \case
            VForAll x cl  -> pure (t, x, cl)
            VFun{}        -> tryForAll =<< (badImplApp r va a >> hole)
            ty            -> tryForAll (t, ty)
        a <- checkTy a
        va <- evalTy' a
        vb <- cl $$$ va
        go (AppI t a) vb as

inferLet :: [R.LocalBind] -> R.Tm -> M (Tm, VTy)
inferLet bs u = case bs of
  []              -> inferTm u
  (x, ma, t) : bs -> do
    a <- maybe (freshNamedMeta "t") checkTy ma
    va <- evalTy' a
    t <- checkTm t va
    (u, vb) <- withVar x va $ inferLet bs u
    pure (Let x a t u, vb)

inferTup :: BegPos -> EndPos -> [R.Tm] -> M (Tm, VTy)
inferTup beg end = \case
  [t] -> inferTm t
  ts  -> do
    vas <- freshNamedMeta' <$$> zipWith (flip const) ts tupleMetaNames
    let va = VTTuple vas
    t <- checkTm (R.Tup ts beg end) va
    pure (t, va)

inferListLit :: BegPos -> EndPos -> [R.Tm] -> M (Tm, VTy)
inferListLit beg end = inferTm . foldr cons nil
  where
    nil = R.Iden dsNil beg end
    cons t u = R.App (R.App (R.Iden dsCons beg end) t) u

inferOverloadedUnary ::
  R.Tm -> OverloadType -> [(VTy, VTy)] -> (Tm -> Tm) -> R.Tm -> M (Tm, VTy)
inferOverloadedUnary r ot zs f rt = do
  (t, va) <- inferTm rt
  forceTy va >>= \case
    VMeta{} -> ambiguousOverloading r ot >> hole
    va      -> go t va zs
  where
    go t va = \case
      []              -> typeNotSupported rt ot va >> hole
      (va', vb) : zs  | va == va' -> pure (f t, vb)
                      | otherwise -> go t va zs

rebindUnary :: Name -> BegPos -> R.Tm -> M (Tm, VTy)
rebindUnary x beg t = inferTm $ R.App (R.Iden x beg (R.endOf t)) t

inferOverloadedBinary ::
  R.Tm -> OverloadType -> [(VTy, VTy)] -> (Tm -> Tm -> Tm) ->
  R.Tm -> R.Tm -> M (Tm, VTy)
inferOverloadedBinary r ot zs f rt ru = do
  (t, va) <- inferTm rt
  forceTy va >>= \case
    VMeta{} -> do
      (u, vb) <- inferTm ru
      forceTy vb >>= \case 
        VMeta{} -> ambiguousOverloading r ot >> hole
        vb      -> goSecond t va u vb zs
    va      -> goFirst t va zs
  where
    goFirst t va = \case
      []              -> typeNotSupported rt ot va >> hole
      (va', vb) : zs  | va == va' -> (\u -> (f t u, vb)) <$> checkTm ru va
                      | otherwise -> goFirst t va zs
    goSecond t va u vb = \case
      []              -> typeNotSupported ru ot vb >> hole
      (vb', vc) : zs  | vb == vb' -> unify rt vb va >> pure (f t u, vc)
                      | otherwise -> goSecond t va u vb zs

rebindBinary :: Name -> R.Tm -> R.Tm -> M (Tm, VTy)
rebindBinary x t u =
  inferTm $ R.App (R.App (R.Iden x (R.begOf t) (R.endOf u)) t) u

rebindOverloadedBinary ::
  R.Tm -> OverloadType -> [(VTy, Name)] -> R.Tm -> R.Tm -> M (Tm, VTy)
rebindOverloadedBinary r ot zs rt ru = do
  (t, va) <- inferTm rt
  forceTy va >>= \case
    VMeta{} -> do
      (u, vb) <- inferTm ru
      forceTy vb >>= \case 
        VMeta{} -> ambiguousOverloading r ot >> hole
        vb      -> goSecond t va u vb zs
    va      -> goFirst t va zs
  where
    goFirst t va = \case
      []            -> typeNotSupported rt ot va >> hole
      (va', x) : zs | va == va' -> rebind1 x t va
                    | otherwise -> goFirst t va zs
    goSecond t va u vb = \case
      []            -> typeNotSupported ru ot va >> hole
      (vb', x) : zs | vb == vb' -> rebind2 x t u va vb
                    | otherwise -> goSecond t va u vb zs
    rebind1 x t va = do
      (f, vf) <- insertImpl' =<< inferTm (R.Iden x (R.begOf rt) (R.endOf ru))
      (f, va', vb) <- ensureFun r f vf
      unify rt va' va
      (t, vb) <- insertImpl' (App f t, vb)
      (t, vb, vc) <- ensureFun r t vb
      u <- checkTm ru vb
      pure (App t u, vc)
    rebind2 x t u va vb = do
      (f, vf) <- insertImpl' =<< inferTm (R.Iden x (R.begOf rt) (R.endOf ru))
      (f, va', vb') <- ensureFun r f vf
      unify rt va' va
      (t, vb') <- insertImpl' (App f t, vb')
      (t, vb', vc) <- ensureFun r t vb'
      unify ru vb' vb
      pure (App t u, vc)

inferUnOp :: R.Tm -> R.UnaryOp -> R.Tm -> M (Tm, VTy)
inferUnOp r op t = 
  let (_, s) = R.unOpDetails op
  in case op of
    R.Plus  -> inferOverloadedUnary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 id t
    R.Minus -> inferOverloadedUnary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)] 
                 (UnOp Negate) t
    R.BNot  -> (UnOp BNot &&& const VTInt) <$> checkTm t VTInt
    R.Not   -> rebindUnary dsNot (R.begOf r) t
    R.Pure  -> (UnOp Pure *** VWorld) <$> inferTm t

inferBinOp :: R.Tm -> R.BinaryOp -> R.Tm -> R.Tm -> M (Tm, VTy)
inferBinOp r op t u =
  let (_, _, s) = R.binOpDetails op
  in case op of
    R.Add   -> inferOverloadedBinary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 (BinOp Add) t u
    R.Sub   -> inferOverloadedBinary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 (BinOp Sub) t u
    R.Mul   -> inferOverloadedBinary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 (BinOp Mul) t u
    R.Div   -> inferOverloadedBinary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 (BinOp Div) t u
    R.Exp   -> inferOverloadedBinary r (OTOperator s)
                 [(VTInt, VTInt), (VTFloat, VTFloat)]
                 (BinOp Exp) t u
    R.Eq    -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsEqInt), (VTFloat, dsEqFloat), (VTChar, dsEqChar)]
                 t u
    R.Neq   -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsNeqInt), (VTFloat, dsNeqFloat), (VTChar, dsNeqChar)]
                 t u
    R.Lt    -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsLtInt), (VTFloat, dsLtFloat), (VTChar, dsLtChar)]
                 t u
    R.Lte   -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsLteInt), (VTFloat, dsLteFloat), (VTChar, dsLteChar)]
                 t u
    R.Gt    -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsGtInt), (VTFloat, dsGtFloat), (VTChar, dsGtChar)]
                 t u
    R.Gte   -> rebindOverloadedBinary r (OTOperator s)
                 [(VTInt, dsGteInt), (VTFloat, dsGteFloat), (VTChar, dsGteChar)]
                 t u
    R.BAnd  -> do
      t <- checkTm t VTInt
      u <- checkTm u VTInt
      pure (BinOp BAnd t u, VTInt)
    R.BOr   -> do
      t <- checkTm t VTInt
      u <- checkTm u VTInt
      pure (BinOp BOr t u, VTInt)
    R.And   -> rebindBinary dsAnd t u
    R.Or    -> rebindBinary dsOr t u
    R.Cons  -> rebindBinary dsCons t u
    R.Seq   -> rebindBinary dsSeq t u

inferUnFunc :: R.Tm -> R.UnaryFunc -> R.Tm -> M (Tm, VTy)
inferUnFunc r f t =
  let x = R.unFuncName f
  in case f of
    R.ToInt   -> inferOverloadedUnary r (OTFunction x)
                   [(VTInt, VTInt), (VTFloat, VTInt), (VTChar, VTInt)]
                   (UnFunc ToInt) t
    R.ToFloat -> inferOverloadedUnary r (OTFunction x)
                   [(VTInt, VTFloat), (VTFloat, VTFloat)]
                   (UnFunc ToFloat) t
    R.ToChar  -> inferOverloadedUnary r (OTFunction x)
                   [(VTInt, VTChar), (VTChar, VTChar)]
                   (UnFunc ToChar) t
    R.Error   -> do
      (t, _) <- inferTm t
      pure (UnFunc Error t, VForAll "a" $ TClosure [] 0)
    R.Putc    -> (UnFunc Putc &&& const (VWorld VUnit)) <$> checkTm t VTChar
    R.Print   -> (UnFunc Print *** const (VWorld VUnit)) <$> inferTm t

inferBinFunc :: R.Tm -> R.BinaryFunc -> R.Tm -> R.Tm -> M (Tm, VTy)
inferBinFunc r f t u =
  let x = R.binFuncName f
  in case f of
    R.PrimEq  -> inferOverloadedBinary r (OTFunction x)
                   [(VTInt, VTInt), (VTFloat, VTInt), (VTChar, VTInt)]
                   (BinFunc PrimEq) t u
    R.PrimLt  -> inferOverloadedBinary r (OTFunction x)
                   [(VTInt, VTInt), (VTFloat, VTInt), (VTChar, VTInt)]
                   (BinFunc PrimLt) t u

inferSwitch :: BegPos -> EndPos -> [R.SwitchBranch] -> M (Tm, VTy)
inferSwitch beg end = inferTm . foldr addBranch err
  where
    msg = "switch overflow"
    err = R.UnFunc R.Error (R.StringLit msg beg end) beg
    addBranch (t, u) r =
      R.Case t
        [ (R.CtrPat dsTrue [] (R.begOf t) (R.endOf t), u)
        , (R.WildcardPat (R.endOf u), r) ]
        (R.begOf t) (R.endOf u)

inferDo :: [R.LocalBind] -> R.Tm -> M (Tm, VTy)
inferDo bs u = case bs of
  []              -> inferTm u
  (x, ma, t) : bs -> do
    a <- maybe (freshNamedMeta "t") checkTy ma
    va <- VWorld <$> evalTy' a
    t <- checkTm t va
    (u, vb) <- withVar x va $ inferDo bs u
    pure (Bind x a t u, vb)

-- | Infer the type of a term.
inferTm :: R.Tm -> M (Tm, VTy)
inferTm r = case r of
  R.Iden x _ _          -> inferIden r x
  R.Lam bs t _          -> inferLam bs t
  R.App t u             -> inferApp r t u
  R.AppI t as _         -> inferAppI r t as
  R.Let bs t _          -> inferLet bs t
  R.TyAnn t a           -> do
    a <- checkTy a
    va <- evalTy' a
    t <- checkTm t va
    pure (t, va)
  R.Hole _              -> do
    va <- freshNamedMeta' "t"
    (Hole, va) <$ newHole r va
  R.IntLit n _ _        -> pure (IntLit n, VTInt)
  R.FloatLit n _ _      -> pure (FloatLit n, VTFloat)
  R.CharLit c _ _       -> pure (CharLit c, VTChar)
  R.StringLit s beg end ->
    let ts = map (\c -> R.CharLit c beg end) s
    in  inferListLit beg end ts
  R.Tup ts beg end      -> inferTup beg end ts
  R.ListLit ts beg end  -> inferListLit beg end ts
  R.UnOp op t _         -> inferUnOp r op t
  R.BinOp op t u        -> inferBinOp r op t u
  R.NullFunc f _ _      -> pure $ case f of
    R.Read  -> (NullFunc Read , VWorld VTChar)
    R.Peek  -> (NullFunc Peek , VWorld VTChar)
    R.IsEOF -> (NullFunc IsEOF, VWorld VTInt)
  R.UnFunc f t _        -> inferUnFunc r f t
  R.BinFunc f t u _     -> inferBinFunc r f t u
  R.If t u v beg        ->
    inferTm $ R.App (R.App (R.App (R.Iden dsIfThenElse beg (R.endOf v)) t) u) v
  R.Split t xs u _      -> do
    (t, zs) <- checkSplitScrutinee t xs
    (u, vb) <- foldr (\(x, va) f -> withVar x va . f) id zs $ inferTm u
    pure (Case t [(PTuple xs, u)], vb)
  R.Switch bs beg end   -> inferSwitch beg end bs
  R.Case t bs _ _       -> do
    (t, zs) <- checkCasePatterns t bs
    va <- freshNamedMeta' "t"
    (, va) <$> checkCaseTerms t zs va
  R.Do bs t _           -> inferDo bs t

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
    checkForErrors $ do
      t <- checkTm t va
      f (TL l a t)
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
      []      -> atTopLevel $ f (DD l xs (reverse cs))
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
checkProgram :: R.Program -> M (Program, ElabCxt)
checkProgram = go pure
  where
    go f = \case
      []      -> pair (f []) getElabCxt
      d : ds  -> case d of
        Left td   -> withTypeDecl td $ \td -> go (\p -> f (Left td : p)) ds
        Right tl  -> withTopLevelDef tl $ \tl -> go (\p -> f (Right tl : p)) ds
