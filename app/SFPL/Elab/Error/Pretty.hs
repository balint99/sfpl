{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, ViewPatterns #-}

-- | Pretty-printing elaboration errors.
module SFPL.Elab.Error.Pretty where

import Prelude hiding ((<>))

import Data.Array.IArray (Array, (!))
import Data.Function (on)
import qualified Data.HashMap.Lazy as M
import Data.List (nub)
import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Error.Types
import SFPL.Elab.Metacontext
import SFPL.Syntax.Core
import SFPL.Syntax.Raw.Types (BegPos, EndPos)
import Text.Megaparsec.Pos
import Text.PrettyPrint

-- | Pretty-print a source position.
prettySourcePos :: SourcePos -> Doc
prettySourcePos (SourcePos fileName (unPos -> lnum) (unPos -> cnum))
  = text fileName <> colon <> int lnum <> colon <> int cnum

-- | Pretty-print a given type of elaboration error.
prettyErrorType :: TyPCxt -> ElabErrorType -> Doc
prettyErrorType tcxt = \case
  NotInScopeError x cat ->
    let s = case cat of
          SCType        -> "Type"
          SCVariable    -> "Variable"
          SCConstructor -> "Data constructor"
    in  text s <+> text "not in scope:" <+> quotes (text x)
  MultipleDeclarationsError x prev ->
      text "Multiple declarations of" <+> quotes (text x)
   $$ text "Previous declaration was at" <+> prettySourcePos prev
  UnificationError expected actual reason ->
        text "Couldn't match expected type" <+> quotes (pretty tcxt expected)
    <+> text "with actual type" <+> quotes (pretty tcxt actual)
  AmbiguousOverloading otype ->
    let (s, x) = case otype of
          OTOperator op -> ("operator", op)
          OTFunction x  -> ("built-in function", x)
    in  text "Ambiguous use of" <+> text s <+> quotes (text x)
  HoleError a ->
    text "Found hole: _ :" <+> pretty tcxt a

prettyBinding :: TyPCxt -> (Name, Ty) -> Doc
prettyBinding tcxt (x, a) = text x <+> char ':' <+> pretty tcxt a

-- | Pretty-print an elaboration error item.
prettyErrorItem :: TyPCxt -> ElabCxt -> ElabErrorItem -> Doc
prettyErrorItem tcxt cxt = \case
  Bindings  ->
    let xs = tmVars $ printInfo cxt
        ns = terms $ names cxt
        bindings = map (prettyBinding tcxt . withType) $ nub xs
          where
            withType x = (x, tmEntryType $ ns M.! x)
    in  case bindings of
          []  -> text "Local bindings in scope: none"
          _   -> text "Local bindings in scope:"
              $$ nest 2 (vcat bindings)

-- | The contents of a source file as an array of lines.
type SourceFile = Array Int String

-- | Pretty-print the offending line.
prettyOffendingLine :: SourceFile -> BegPos -> EndPos -> Doc
prettyOffendingLine src beg end =
  let SourcePos _ (unPos -> begLine) (unPos -> begCol) = beg
      SourcePos _ (unPos -> endLine) (unPos -> endCol) = end
      lineNumber = show begLine
      padding = text $ map (const ' ') lineNumber
      line = src ! (begLine - 1)
      marker = if begLine == endLine
        then text $ replicate (endCol - begCol) '^'
        else text (replicate (length line - begCol + 2) '^') <> text "..."
  in  padding <+> char '|'
   $$ text lineNumber <+> char '|' <+> text line
   $$ padding <+> char '|' <+> text (replicate (begCol - 1) ' ') <> marker

-- | Pretty-print an elaboration error, given the contents of the source file.
--
-- @since 1.0.0
prettyElabError :: SourceFile -> ElabError -> Doc
prettyElabError src (ElabError cxt metas errorSpan errorType errorItems) =
  let PrintCxt xs ts _ _ _ = printInfo cxt
      tcxt = tyPCxt xs (toAssocList (metaName . snd) metas) ts
      (beg, end) = errorSpan
      theError = prettyErrorType tcxt errorType
      items = map (prettyErrorItem tcxt cxt) errorItems
      body = case items of
        []  -> theError
        _   -> vcat . map (\d -> char '*' $$ nest 2 d) $ theError : items
      offendingLine = prettyOffendingLine src beg end
  in  prettySourcePos beg <> text ": error:"
   $$ nest 4 body
   $$ offendingLine

instance Pretty SourceFile ElabError where
  pretty = prettyElabError
