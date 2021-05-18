{-# LANGUAGE FlexibleInstances #-}
module SFPL.Elab.Error
  ( module SFPL.Elab.Error.Types,
    module SFPL.Elab.Error.Pretty,
    module SFPL.Elab.Error,
  )
  where

import SFPL.Elab.Error.Types
import SFPL.Elab.Error.Pretty

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import SFPL.Base
import SFPL.Utils
import SFPL.Elab.Metacontext
import SFPL.Syntax.Core
import Text.Megaparsec.Pos
import SFPL.Elab.Context
import Text.PrettyPrint
import System.IO

instance Metas (HashMap Metavar MetaEntry) where
  metasGet = flip (M.!)
  metasToAssocList f = map (fmap f) . M.toList

fileName = "asd.hs"
beg = SourcePos fileName (mkPos 1) (mkPos 5)
end = SourcePos fileName (mkPos 1) (mkPos 6)
tlcxt = TopLevelCxt 0 0 0
ns = Namespaces M.empty (M.fromList [ ("x", VarEntry 1 (Meta 0 []))
                                    , ("y", VarEntry 4 Int)
                                    , ("z", VarEntry 3 (TyVar 1))])
pcxt = PrintCxt ["b", "a"] [] [] ["y", "z", "y", "x", "x"] []
cxt = ElabCxt tlcxt ns pcxt [] 0 0
mcxt = SomeMetas (M.fromList $ [(0 :: Metavar, (Unsolved, MetaInfo 0 "a0"))])

getInput :: IO String
getInput = do
  b <- isEOF
  if b
    then pure ""
    else do
      l <- getLine
      ls <- getInput
      pure (l ++ '\n' : ls)

testError :: IO ()
testError = do
  file <- getInput
  let src = arr $ lines file :: SourceFile
      err = ElabError cxt mcxt (beg, end)
        (HoleError Int) [Bindings]
  putStrLn . show $ pretty src err
