{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, RecordWildCards #-}
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
import Control.Monad.Reader
import SFPL.Base
import SFPL.Utils
import SFPL.Parser
import SFPL.Elab.Context
import SFPL.Elab.Metacontext
import SFPL.Syntax.Core
import SFPL.Syntax.Core.Pretty
import qualified SFPL.Syntax.Raw as R
import SFPL.Elab.Error
import SFPL.Elab.Class
import SFPL.Elab.Unification
import SFPL.Elab.Internal
import SFPL.Eval.Types
import qualified SFPL.Eval as E
import Text.PrettyPrint
import Prelude hiding ((<>))
import Text.Megaparsec hiding (State)
import Text.PrettyPrint
import Data.Void

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

instance Num Pos where
  fromInteger = mkPos . fromInteger
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined

type Elab = ReaderT ElabCxt (ExceptT [ElabError] (State ElabSt))

instance MonadMeta Elab where
  freshMeta info = do
    st <- get
    let ElabSt {..} = st
    let metaEntries' = M.insert nextMeta (Unsolved, info) metaEntries
        nextMeta' = nextMeta + 1
        st = ElabSt {metaEntries = metaEntries', nextMeta = nextMeta', ..}
    put st
    pure $ FreshMeta nextMeta
  
  lookupMeta m = do
    st <- get
    case M.lookup m (metaEntries st) of
      Nothing     -> devError $ "metavariable not in scope: " ++ show m
      Just entry  -> pure entry
  
  updateMeta m a = do
    st <- get
    let ElabSt {..} = st
    case M.lookup m metaEntries of
      Nothing -> devError $ "metavariable not in scope: " ++ show m
      Just _  -> do
        let f (_, info) = (Solved a, info)
            metaEntries' = M.adjust f m metaEntries
            st = ElabSt {metaEntries = metaEntries', ..}
        put st
  
  getMetas = do
    SomeMetas . metaEntries <$> get

instance MonadElab Elab where
  getElabCxt = ask
  withElabCxt = local
  
  registerElabError err = do
    st <- get
    let ElabSt {..} = st
    let elabErrors' = err : elabErrors
        st = ElabSt {elabErrors = elabErrors', ..}
    put st

  throwElabErrors = do
    st <- get
    let errors = elabErrors st
    case errors of
      []  -> devError "no registered elaboration errors"
      _   -> throwError $ reverse errors

  freshName x = do
    st <- get
    let ElabSt {..} = st
    let n = M.findWithDefault 0 x metaCounters
        metaCounters' = M.insert x (n + 1) metaCounters
        st = ElabSt {metaCounters = metaCounters', ..}
    put st
    pure $ '?' : x ++ show n

initCxt = ElabCxt
  { topLevelCxt = TopLevelCxt 4 7 1
  , names = Namespaces (M.fromList $ [ ("maybe"
                                       , DataEntry 0 1 (SourcePos "<stdin>" 1 1)
                                       )
                                     , ( "T"
                                       , DataEntry 1 0 (SourcePos "<stdin>" 8 1)
                                       )
                                     , ( "list"
                                       , DataEntry 2 1 (SourcePos "<stdin>" 11 1)
                                       )
                                     , ( "either"
                                       , DataEntry 3 2 (SourcePos "<stdin>" 15 1)
                                       )
                                     ])
                       (M.fromList $ [ ( "nothing"
                                       , ConstructorEntry 0
                                           (Data 0 [0]) ["a"]
                                           (SourcePos "<stdin>" 2 4)
                                       )
                                     , ( "just"
                                       , ConstructorEntry 1
                                           (Fun 0 (Data 0 [0])) ["a"]
                                           (SourcePos "<stdin>" 3 4)
                                       )
                                     , ( "id"
                                       , TopLevelEntry 0
                                           (VForAll "a" $ TClosure [] $ Fun 0 0)
                                           (SourcePos "<stdin>" 5 1)
                                       )
                                     , ( "T"
                                       , ConstructorEntry 2
                                           (ForAll "a" $ ForAll "b" $
                                             Fun (Tuple [1, 0]) (Data 1 [])) []
                                           (SourcePos "<stdin>" 9 4)
                                       )
                                     , ( "nil"
                                       , ConstructorEntry 3
                                           (Data 2 [0]) ["a"]
                                           (SourcePos "<stdin>" 12 4)
                                       )
                                     , ( "cons"
                                       , ConstructorEntry 4
                                           (Fun 0 $ Fun (Data 2 [0]) $ Data 2 [0]) ["a"]
                                           (SourcePos "<stdin>" 13 4)
                                       )
                                     , ( "left"
                                       , ConstructorEntry 5
                                           (Fun 1 $ Data 3 [0, 1]) ["a", "b"]
                                           (SourcePos "<stdin>" 16 4)
                                       )
                                     , ( "right"
                                       , ConstructorEntry 6
                                           (Fun 0 $ Data 3 [0, 1]) ["a", "b"]
                                           (SourcePos "<stdin>" 17 4)
                                       )  
                                     ])
  , printInfo = PrintCxt [] ["either", "list", "T", "maybe"]
                            ["right", "left", "cons", "nil", "T", "just", "nothing"] [] ["id"]
  , tyEnv = []
  , tyLvl = 0
  , tmLvl = 0
  }

emptySt = ElabSt
  { metaEntries = M.empty
  , nextMeta = 0
  , elabErrors = []
  , metaCounters = M.empty
  }

instance {-# OVERLAPPING #-} MonadFail Elab where
  fail = error

runElab :: Elab a -> (Either [ElabError] (a, PrintCxt), ElabSt)
runElab m =
  runState (runExceptT (runReaderT (pair m (printInfo <$> getElabCxt)) initCxt)) emptySt

elabTest :: Parser a -> (a -> Elab b) -> (b -> PrintCxt -> SomeMetas -> String) -> IO ()
elabTest p f g = do
  s <- readInput
  let res = parse (topLevel p) "<stdin>" s
  case res of
    Left bundle -> putStr $ errorBundlePretty bundle
    Right a -> do
      let (res, st) = runElab (f a)
          src = arr $ lines s :: SourceFile
          metas = SomeMetas $ metaEntries st
      case res of
        Left errs -> putStr . render . vcat $ map (($$ text "") . pretty (src, metas)) errs
        Right (b, pcxt) -> putStrLn $ g b pcxt metas

elabTestTy = elabTest ty checkTy printTy
  where
    printTy a (PrintCxt xs ts _ _ _) metas =
      let tcxt = tyPCxt xs (toAssocList (metaName . snd) metas) (reverse ts)
      in showPretty tcxt a

elabTestCtrTy x xs y = elabTest ty elabCtrTy printTy
  where
    elabCtrTy r = do
      Just (DataEntry l _ _) <- lookupTy x
      foldl (\f x -> f . withTyVar x) id xs $ checkCtrTy y l r
    printTy a (PrintCxt _ ts _ _ _) metas =
      let tcxt = tyPCxt (reverse xs) (toAssocList (metaName . snd) metas) (reverse ts)
      in showPretty tcxt a

elabTestPat = elabTest pat inferPat printPat
  where
    printPat (p, va, bindings) (PrintCxt xs ts cs _ _) metas =
      let pcxt = patPCxt (reverse cs)
          tcxt = tyPCxt xs (toAssocList (metaName . snd) metas) (reverse ts)
          prettyBinding tcxt = \case
            Left (x, va)  -> text x <+> colon <+> pretty (tcxt, metas) va
            Right x       -> text x <+> colon <+> char '*'
          prettyBindings tcxt = \case
            []      -> (Text.PrettyPrint.empty, tcxt)
            b : bs  -> let (d, tcxt') = prettyBindings (newBinding b tcxt) bs
                       in  (prettyBinding tcxt b $$ d, tcxt')
            where
              newBinding b tcxt = case b of
                Left _  -> tcxt
                Right x -> tyBind x tcxt
          (d, tcxt') = prettyBindings tcxt bindings
      in render $
             pretty pcxt p <+> colon <+> pretty (tcxt', metas) va
          $$ nest 2 d