{-# LANGUAGE FlexibleInstances, LambdaCase, RecordWildCards #-}

-- | Elaboration.
module SFPL.Elab
  ( -- * Reexports
    module SFPL.Elab.Error,
    module SFPL.Elab.Metacontext,
    module SFPL.Elab.Unification,
    
    -- * Elaboration monad class
    MonadElab (..),
    
    -- * Elaboration context
    TopLevelCxt (..),
    Namespaces (..),
    TyEntry (..),
    TmEntry (..),
    PrintCxt (..),
    ElabCxt (..),
    
    -- * Checking and inferring
    checkTy,
    checkTy',
    PatternBindings,
    checkPat,
    inferPat,
    checkTm,
    inferTm,
    checkProgram,
    checkMainProgram,
    
    -- * Elaboration
    Elab,
    ElabSt (..),
    
    -- ** Functions
    emptyElabCxt,
    emptyElabSt,
    runElab,
    elabProgram,
  )
  where

import SFPL.Elab.Class
import SFPL.Elab.Context
import SFPL.Elab.Error
import SFPL.Elab.Metacontext
import SFPL.Elab.Unification
import SFPL.Elab.Instances
import qualified SFPL.Elab.Internal as I

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import SFPL.Base
import SFPL.Eval
import SFPL.Syntax.Core
import qualified SFPL.Syntax.Raw as R
import SFPL.Utils

----------------------------------------
-- Helpers

-- Note: we redefine the checking and inferring functions
-- in this module so that their types don't contain
-- internal type synonym 'M'.

-- | Check that a type is well-formed.
-- Creates fresh metavariables on holes.
--
-- @since 1.0.0
checkTy :: MonadElab m => R.Ty -> m Ty
checkTy = I.checkTy

-- | Check that a type is well-formed. The first parameter gives
-- the action to perform on holes.
--
-- @since 1.0.0
checkTy' :: MonadElab m => m Ty -> R.Ty -> m Ty
checkTy' = I.checkTy'

-- | The bindings that a pattern establishes.
-- A binding is either a term or type variable binding.
-- For terms, their type is also stored.
-- For types, the binding is either explicit or implicit.
--
-- @since 1.0.0
type PatternBindings = I.PatternBindings

-- | Check that a pattern has the given type.
-- Returns the list of variables the pattern binds in a 'Just',
-- or 'Nothing' if checking fails.
--
-- @since 1.0.0
checkPat :: MonadElab m => R.Pattern -> VTy -> m (Maybe (Pattern, PatternBindings))
checkPat = I.checkPat

-- | Infer the type of a pattern.
-- Returns the inferred type and the list of variables
-- the pattern binds.
inferPat :: MonadElab m => R.Pattern -> m (Maybe (Pattern, VTy, PatternBindings))
inferPat = I.inferPat

-- | Check that a term has the given type.
--
-- @since 1.0.0
checkTm :: MonadElab m => R.Tm -> VTy -> m Tm
checkTm = I.checkTm

-- | Infer the type of a term.
--
-- @since 1.0.0
inferTm :: MonadElab m => R.Tm -> m (Tm, VTy)
inferTm = I.inferTm

-- | Elaborate a program.
-- Returns the checked program, as well as
-- the state of the elaboration context at the end.
--
-- @since 1.0.0
checkProgram :: MonadElab m => R.Program -> m (Program, ElabCxt)
checkProgram = I.checkProgram

-- | Elaborate a program and the check for the
-- presence of the main function. Returns the checked
-- program, the top-level identifier of the main function
-- and the state of the elaboration context at the end.
--
-- @since 1.0.0
checkMainProgram :: MonadElab m => R.Program -> m (Program, Lvl, ElabCxt)
checkMainProgram = I.checkMainProgram

------------------------------------------------------------
-- Concrete elaboration monad

-- | Elaboration state.
--
-- @since 1.0.0
data ElabSt = ElabSt
  { -- | The current metavariables.
    metaEntries :: HashMap Metavar MetaEntry
  , -- | The ordinal of the next metavariable.
    nextMeta :: Metavar
  , -- | Registered elaboration errors.
    elabErrors :: [ElabError]
  , -- | A map which contains for each base name the next numeral that is
    -- going to be used for fresh metavariable creation. A missing name implies
    -- that a metavariable with that base name has not yet been created.
    metaCounters :: HashMap TyName Int
  }

-- | @since 1.0.0
instance Metas (HashMap Metavar MetaEntry) where
  metasGet = flip (M.!)
  metasToAssocList f = map (fmap f) . M.toList

-- | Elaboration monad.
--
-- @since 1.0.0
type Elab = ReaderT ElabCxt (ExceptT [ElabError] (State ElabSt))

-- | Create a fresh metavariable using the given information.
freshMetaElab :: MetaInfo -> Elab Ty
freshMetaElab info = do
  st <- get
  let ElabSt {..} = st
      metaEntries' = M.insert nextMeta (Unsolved, info) metaEntries
      nextMeta' = nextMeta + 1
  let st = ElabSt {metaEntries = metaEntries', nextMeta = nextMeta', ..}
  put st
  pure $ FreshMeta nextMeta

-- | Lookup a metavariable in the metacontext.
-- Precondition: the metavariable exists.
lookupMetaElab :: Metavar -> Elab MetaEntry
lookupMetaElab m = do
  st <- get
  case M.lookup m (metaEntries st) of
    Nothing     -> devError $ "metavariable not in scope: " ++ show m
    Just entry  -> pure entry

-- | Update a metavariable with a solution.
-- Precondition: the metavariable exists.
updateMetaElab :: Metavar -> Ty -> Elab ()
updateMetaElab m a = do
  st <- get
  let ElabSt {..} = st
  case M.lookup m metaEntries of
    Nothing -> devError $ "metavariable not in scope: " ++ show m
    Just _  -> do
      let f (_, info) = (Solved a, info)
          metaEntries' = M.adjust f m metaEntries
          st = ElabSt {metaEntries = metaEntries', ..}
      put st

-- | @since 1.0.0
instance MonadMeta Elab where
  freshMeta = freshMetaElab
  lookupMeta = lookupMetaElab
  updateMeta = updateMetaElab
  getMetas = someMetas . metaEntries <$> get

-- | Register an elaboration error.
registerElabErrorElab :: ElabError -> Elab ()
registerElabErrorElab err = do
  st <- get
  let ElabSt {..} = st
      elabErrors' = err : elabErrors
  let st = ElabSt {elabErrors = elabErrors', ..}
  put st

-- | Throw the registered elaboration errors.
-- Precondition: there is at least one registered error.
throwElabErrorsElab :: Elab a
throwElabErrorsElab = do
  errors <- elabErrors <$> get
  case errors of
    []  -> devError "no registered elaboration errors"
    _   -> throwError $ reverse errors

-- | Find a fresh name for a metavariable using
-- the given base name.
freshNameElab :: TyName -> Elab TyName
freshNameElab x = do
  st <- get
  let ElabSt {..} = st
      n = M.findWithDefault 0 x metaCounters
      metaCounters' = M.insert x (n + 1) metaCounters
  let st = ElabSt {metaCounters = metaCounters', ..}
  put st
  pure $ '?' : x ++ show n

-- | @since 1.0.0
instance MonadElab Elab where
  getElabCxt = ask
  withElabCxt = local
  registerElabError = registerElabErrorElab
  isErrorRegistered = not . null . elabErrors <$> get
  throwElabErrors = throwElabErrorsElab
  freshName = freshNameElab

----------------------------------------
-- Functions

-- | The empty elaboration context.
--
-- @since 1.0.0
emptyElabCxt = ElabCxt
  { topLevelCxt = TopLevelCxt 0 0 0
  , names = Namespaces M.empty M.empty
  , printInfo = PrintCxt [] [] [] [] []
  , tyEnv = []
  , tyLvl = 0
  , tmLvl = 0
  }

-- | The empty elaboration state.
--
-- @since 1.0.0
emptyElabSt :: ElabSt
emptyElabSt = ElabSt
  { metaEntries = M.empty
  , nextMeta = 0
  , elabErrors = []
  , metaCounters = M.empty
  }

-- | Run the given elaboration computation starting from
-- the empty context and empty state, and return the result
-- along with the final state of the metacontext.
-- The result may be a bunch of elaboration errors.
--
-- @since 1.0.0
runElab :: Elab a -> (Either [ElabError] a, SomeMetas)
runElab m =
  let (res, st) = runState (runExceptT (runReaderT m emptyElabCxt)) emptyElabSt
  in (res, someMetas $ metaEntries st)

-- | Fully elaborate a complete program, preparing it
-- for evaluation. Returns the prepared evaluation context
-- and the top-level identifier of the main function on success
-- or a bunch of elaboration errors on failure, as well as
-- the final state of the metacontext.
--
-- @since 1.0.0
elabProgram :: R.Program -> (Either [ElabError] (EvalCxt, Lvl), SomeMetas)
elabProgram r =
  let (res, metas) = runElab (checkMainProgram r)
  in (prepare <$> res, metas)
  where
    prepare (prog, l, cxt) =
      let tls = map getDef $ rights prog
          cs = reverse . constructorNames $ printInfo cxt
      in (evalCxt tls cs, l)
    getDef (TL _ _ t) = t
