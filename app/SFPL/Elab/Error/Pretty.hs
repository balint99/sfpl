{-# LANGUAGE LambdaCase, ViewPatterns #-}

-- | Pretty-printing elaboration errors.
module SFPL.Elab.Error.Pretty where

import Prelude hiding ((<>))

import Data.Array.IArray (Array, (!))
import Data.Char (toUpper)
import qualified Data.HashMap.Lazy as M
import Data.List (nub)
import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Error.Types
import SFPL.Elab.Metacontext
import SFPL.Eval (VTyPCxt, VTy)
import SFPL.Syntax.Core
import SFPL.Syntax.Raw.Instances
import SFPL.Syntax.Raw.Types (BegPos, EndPos)
import SFPL.Utils
import Text.Megaparsec.Pos
import Text.PrettyPrint

-- | Pretty-print a source position.
prettySourcePos :: SourcePos -> Doc
prettySourcePos (SourcePos fileName (unPos -> lnum) (unPos -> cnum))
  = text fileName <> colon <> int lnum <> colon <> int cnum

-- | Pretty-print a given type of elaboration error.
prettyErrorType :: TyPCxt -> ElabCxt -> SomeMetas -> ElabErrorType -> Doc
prettyErrorType tcxt cxt metas errorType =
  let vcxt = (tcxt, metas)
  in case errorType of
    NotInScopeError x cats ->
      let ns = map scName cats
          prettyNames (n : ns) = text (capitalize n) <> prettyNames' ns
            where
              prettyNames' = \case
                []      -> empty
                [n]     -> text " or" <+> text n
                n : ns  -> comma <+> prettyNames' ns
      in  prettyNames ns <+> text "not in scope:" <+> quotes (text x)
    MultipleDeclarationsError x prev ->
        text "Multiple declarations of" <+> quotes (text x)
     $$ text "Previous declaration was at" <+> prettySourcePos prev
    MalformedTypeError a reason ->
      let explanation = case reason of
            IllegalApplication        ->
              text "Only data types may be applied to arguments"
            BadDataApplication x n n' ->
                text "Bad number of arguments applied to" <+> quotes (text x)
             $$ text "Expected" <+> int n <> text ", got" <+> int n'
      in  text "Malformed type" <+> quotes (pretty' a)
       $$ explanation
    InvalidTypeHoleError a place ->
      let s = case place of
            THPTopLevelDef  -> "top-level definitions"
            THPConstructor  -> "data constructors"
      in  text "Invalid type hole in" <+> quotes (pretty' a)
       $$ text "Type holes are not allowed the in type signatures of" <+> text s
    BadConstructorType x a ->
          text "Invalid return type" <+> quotes (pretty' a)
      <+> text "for data constructor" <+> quotes (text x)
       $$ text "The return type of a data constructor must be"
       $$ text "its parent data type applied to its type parameters"
    MalformedPatternError pat ->
      text "Malformed pattern" <+> quotes (pretty' pat)
    UnificationError vexp vact reason ->
      let explanation = case reason of
            RigidMismatch       -> empty
            InvalidSpine        -> empty
            EscapingVariable i  ->
              text "Variable" <+> quotes (pretty tcxt (TyVar i)) <+> text "escapes its scope"
            Occurs m            ->
              text (nameOf m) <+> text "occurs recursively in the equation"
            where
              nameOf m = metaName . snd $ getMeta m metas
      in  text "Couldn't match expected type" <+> quotes (pretty vcxt vexp)
      <+> text "with actual type" <+> quotes (pretty vcxt vact)
       $$ explanation
    ImplicitApplicationError va b ->
        text "Cannot apply expression of type" <+> quotes (pretty vcxt va)
     $$ text "to a type argument" <+> quotes (pretty' b)
    TypeNotSupportedError ot va ->
      let (s', x) = otHelper ot
          s = capitalize s'
      in text s <+> quotes (text x)
     <+> text "does not support type" <+> quotes (pretty vcxt va)
    AmbiguousOverloadingError ot ->
      let (s, x) = otHelper ot
      in  text "Ambiguous use of" <+> text s <+> quotes (text x)
    HoleError va ->
      text "Found hole: _ :" <+> pretty vcxt va
  where
    capitalize (c : cs) = toUpper c : cs
    otHelper ot = case ot of
      OTOperator op -> ("operator", op)
      OTFunction x  -> ("built-in function", x)

prettyBinding :: VTyPCxt -> (Name, VTy) -> Doc
prettyBinding vcxt (x, va) = text x <+> char ':' <+> pretty vcxt va

-- | Pretty-print an elaboration error item.
prettyErrorItem :: TyPCxt -> ElabCxt -> SomeMetas -> ElabErrorItem -> Doc
prettyErrorItem tcxt cxt metas = \case
  Bindings  ->
    let xs = tmVars $ printInfo cxt
        ns = terms $ names cxt
        vcxt = (tcxt, metas)
        bindings = map (prettyBinding vcxt . withType) . nub $ filter (/= "_") xs
          where
            withType x = (x, getType $ ns M.! x)
            getType = \case
              VarEntry l va -> va
              _             -> devError "cannot print binding: not a variable"
    in  case bindings of
          []  -> text "Local bindings in scope: none"
          _   -> text "Local bindings in scope:"
              $$ nest 2 (vcat bindings)

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

-- | The contents of a source file as an array of lines.
--
-- @since 1.0.0
type SourceFile = Array Int String

-- | Information context for printing errors.
--
-- @since 1.0.0
type ErrorPCxt = (SourceFile, SomeMetas)

-- | Pretty-print an elaboration error, given the contents of the source file
-- and the state of the metacontext at the end of elaboration.
--
-- @since 1.0.0
prettyElabError :: ErrorPCxt -> ElabError -> Doc
prettyElabError (src, metas) (ElabError cxt errorSpan errorType errorItems) =
  let PrintCxt xs ts _ _ _ = printInfo cxt
      tcxt = tyPCxt xs (toAssocList (metaName . snd) metas) (reverse ts)
      (beg, end) = errorSpan
      theError = prettyErrorType tcxt cxt metas errorType
      items = map (prettyErrorItem tcxt cxt metas) errorItems
      body = case items of
        []  -> theError
        _   -> vcat . map (\d -> char '*' $$ nest 2 d) $ theError : items
      offendingLine = prettyOffendingLine src beg end
  in  prettySourcePos beg <> text ": error:"
   $$ nest 4 body
   $$ offendingLine
