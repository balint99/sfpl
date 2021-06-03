{-# LANGUAGE ViewPatterns #-}

-- | Tests for elaboration.
module SFPL.ElabSpec
  ( spec )
  where

import Control.Monad
import SFPL.Base
import SFPL.Elab
import SFPL.Eval (normalizeTypes)
import SFPL.Parser
import SFPL.Syntax.Core
import SFPL.Utils
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

prefix :: FilePath
prefix = "test/file/elab/"

actualPath :: FilePath -> FilePath -> FilePath
actualPath group path =
  prefix ++ group ++ "/" ++ path ++ ".txt"

diag :: a -> (a, a)
diag x = (x, x)

shouldSucceed :: FilePath -> Expectation
shouldSucceed (diag -> ( actualPath "success" . ("in/"  ++) -> inp
                       , actualPath "success" . ("out/" ++) -> out)) = do
  src <- readFile inp
  exp <- readFile out
  let res = parseProgram inp src
  case res of
    Left bundle -> expectationFailure $
      "failed with parse error:\n" ++ errorBundlePretty bundle
    Right prog  -> do
      let src' = arr $ lines src :: SourceFile
          (res, metas) = runElab (checkProgram prog)
      case res of
        Left errors       -> expectationFailure $
          "failed with elaboration error:\n" ++ showPretty (src', metas) errors
        Right (prog, cxt) -> do
          let PrintCxt _ ts cs _ tls = printInfo cxt
              pcxt = progPCxt (reverse tls) (metaNames metas) (reverse ts) (reverse cs)
              prog' = normalizeTypes prog metas
              act = showPretty pcxt prog'
          unless (act == exp) $ expectationFailure $
            unlines [ "expected:"
                    , exp
                    , "but got:"
                    , act ]

shouldFail :: FilePath -> ([ElabError] -> Bool) -> Expectation
shouldFail (actualPath "failure" -> file) p = do
  src <- readFile file
  let res = parseProgram file src
  case res of
    Left bundle -> expectationFailure $
      "failed with parse error:\n" ++ errorBundlePretty bundle
    Right prog  -> do
      let src' = arr $ lines src :: SourceFile 
          (res, metas) = runElab (checkProgram prog)
      case res of
        Left errors       -> unless (p errors) $ expectationFailure $
             "errors didn't satisfy the predicate:\n"
          ++ showPretty (src', metas) errors
        Right (prog, cxt) -> do
          let PrintCxt _ ts cs _ tls = printInfo cxt
              pcxt = progPCxt (reverse tls) (metaNames metas) (reverse ts) (reverse cs)
              prog' = normalizeTypes prog metas
              out = showPretty pcxt prog'
          expectationFailure $
            "expected error, but got:\n" ++ out

spec :: Spec
spec = do
  describe "success" $ do
    it "1" $ shouldSucceed "1"
    it "2" $ shouldSucceed "2"
    it "3" $ shouldSucceed "3"
    it "4" $ shouldSucceed "4"
    it "5" $ shouldSucceed "5"
  
  describe "failure" $ do
    pure ()
