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
        elabErrors' = err : elabErrors
    let st = ElabSt {elabErrors = elabErrors', ..}
    put st

  isErrorRegistered = not . null . elabErrors <$> get
    

  throwElabErrors = do
    st <- get
    let errors = elabErrors st
    case errors of
      []  -> devError "no registered elaboration errors"
      _   -> throwError $ reverse errors

  freshName x = do
    st <- get
    let ElabSt {..} = st
        n = M.findWithDefault 0 x metaCounters
        metaCounters' = M.insert x (n + 1) metaCounters
    let st = ElabSt {metaCounters = metaCounters', ..}
    put st
    pure $ '?' : x ++ show n

forall x = VForAll x . TClosure []

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
                                           (forall "a" $ Data 0 [0]) 1
                                           (SourcePos "<stdin>" 2 4)
                                       )
                                     , ( "just"
                                       , ConstructorEntry 1
                                           (forall "a" $ Fun 0 (Data 0 [0])) 1
                                           (SourcePos "<stdin>" 3 4)
                                       )
                                     , ( "id"
                                       , TopLevelEntry 0
                                           (VForAll "a" $ TClosure [] $ Fun 0 0)
                                           (SourcePos "<stdin>" 5 1)
                                       )
                                     , ( "T"
                                       , ConstructorEntry 2
                                           (forall "a" $ ForAll "b" $
                                             Fun (Tuple [1, 0]) (Data 1 [])) 0
                                           (SourcePos "<stdin>" 9 4)
                                       )
                                     , ( "nil"
                                       , ConstructorEntry 3
                                           (forall "a" $ Data 2 [0]) 1
                                           (SourcePos "<stdin>" 12 4)
                                       )
                                     , ( "cons"
                                       , ConstructorEntry 4
                                           (forall "a" $ Fun 0 $ Fun (Data 2 [0]) $ Data 2 [0]) 1
                                           (SourcePos "<stdin>" 13 4)
                                       )
                                     , ( "left"
                                       , ConstructorEntry 5
                                           (forall "a" $ ForAll "b" $ Fun 1 $ Data 3 [0, 1]) 2
                                           (SourcePos "<stdin>" 16 4)
                                       )
                                     , ( "right"
                                       , ConstructorEntry 6
                                           (forall "a" $ ForAll "b" $ Fun 0 $ Data 3 [0, 1]) 2
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

elabTestCtr l xs = elabTest constructor elabCtr printCtr
  where
    elabCtr r = withTyVars xs $ do
      (c, _, _) <- checkCtr l r
      cxt <- printInfo <$> getElabCxt
      pure (c, cxt)
    printCtr (ctr, PrintCxt xs ts cs _ _) _ _ =
      let ccxt = ctrPCxt xs (reverse ts) (reverse cs)
      in show ctr

elabTestDataDecl = elabTest dataDecl elabDataDecl printDataDecl
  where
    elabDataDecl r = withDataDecl r (\dd -> printInfo <$> getElabCxt >>= \cxt -> pure (dd, cxt))
    printDataDecl (dd, PrintCxt _ ts cs _ _) _ _ =
      let ddcxt = ddPCxt (reverse ts) (reverse cs)
      in showPretty ddcxt dd

elabTestTopLevelDef = elabTest topLevelDef elabTopLevelDef printTopLevelDef
  where
    elabTopLevelDef r = withTopLevelDef r (\tl -> printInfo <$> getElabCxt >>= \cxt -> pure (tl, cxt))
    printTopLevelDef (tl, PrintCxt _ ts cs _ tls) _ _ =
      let tlcxt = tlPCxt tls [] ts cs
      in showPretty tlcxt tl

elabTestProgram = elabTest program elabProgram printProgram
  where
    elabProgram = checkProgram
    printProgram (prog, ElabCxt {..}) _ _ =
      let PrintCxt _ ts cs _ tls = printInfo
          pcxt = progPCxt tls [] ts cs
      in showPretty pcxt prog
