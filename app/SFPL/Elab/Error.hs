{-# LANGUAGE FlexibleInstances #-}
module SFPL.Elab.Error
  ( module SFPL.Elab.Error.Types,
    module SFPL.Elab.Error.Pretty,
    module SFPL.Elab.Error,
  )
  where

import SFPL.Elab.Error.Types
import SFPL.Elab.Error.Pretty
import SFPL.Elab.Error.Instances

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import SFPL.Base
import SFPL.Utils
import SFPL.Elab.Metacontext
import SFPL.Syntax.Core.Types
import SFPL.Eval.Types
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
ns = Namespaces M.empty (M.fromList [ ("x", VarEntry 1 $ VMeta 0 [])
                                    , ("y", VarEntry 4 VTInt)
                                    , ("z", VarEntry 3 $ VTyVar 0)])
pcxt = PrintCxt ["b", "a"] [] [] ["y", "z", "y", "x", "x"] []
cxt = ElabCxt tlcxt ns pcxt [1, 0] 2 3
metas = SomeMetas (M.fromList $ [ (0 :: Metavar, (Unsolved, MetaInfo 0 "a0"))
                                , (1, (Solved $ TyVar 0, MetaInfo 1 "b0"))])

readInput :: IO String
readInput = do
  b <- isEOF
  if b
    then pure ""
    else do
      l <- getLine
      ls <- readInput
      pure (l ++ '\n' : ls)

testError :: IO ()
testError = do
  file <- readInput
  let src = arr $ lines file :: SourceFile
      err = ElabError cxt (beg, end)
        (HoleError (VMeta 1 [1])) [Bindings]
  putStrLn . show $ pretty (src, metas) err
