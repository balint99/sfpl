{-# LANGUAGE LambdaCase #-}

-- | Printing values.
module SFPL.Eval.Pretty where

import Data.Array.IArray ((!))
import Data.Char (showLitChar)
import SFPL.Base
import SFPL.Eval.Types
import SFPL.Utils
import Text.PrettyPrint

----------------------------------------
-- Terms

prettyChar :: Char -> Doc
prettyChar c = text $ showLitChar c ""

prettyCons :: CtrNames -> VSpine -> Doc
prettyCons ctrs ([] :> hd :> tl) = case hd of
  VChar c -> doubleQuotes . hcat . map (prettyChar . toChar) $ hd : toList tl
  _       -> brackets . hjoin ", " . map (prettyVal LowP ctrs) $ hd : toList tl
  where
    toChar :: Val -> Char
    toChar (VChar c) = c
    toChar _ = devError "encountered non-character while printing string"

    toList :: Val -> [Val]
    toList = \case
      VCtr l sp -> nilOrCons (ctrs ! l) sp
      _         -> devError "encountered non-constructor while printing list"
      where
        nilOrCons x sp
          | x == dsNil  = []
          | x == dsCons = case sp of
            [] :> hd :> tl  -> hd : toList tl
            _               -> devError "cons constructor doesn't have 2 fields"
          | otherwise   = devError "encountered non-list constructor while printing list"
prettyCons _ _ = devError "cons constructor doesn't have 2 fields"

prettyCtr :: Prec -> CtrNames -> Name -> VSpine -> Doc
prettyCtr p ctrs x = \case
  []        -> text x
  sp :> vt  -> par p AppP $ prettyCtr AppP ctrs x sp <+> prettyVal AtomP ctrs vt

prettyVCtr :: Prec -> CtrNames -> Name -> VSpine -> Doc
prettyVCtr p ctrs x sp
  | x == dsNil  = brackets empty
  | x == dsCons = prettyCons ctrs sp
  | otherwise   = prettyCtr p ctrs x sp

-- | Pretty-print a term value.
--
-- @since 1.0.0
prettyVal :: Prec -> CtrNames -> Val -> Doc
prettyVal p ctrs = \case
  VLam{}      -> text "<function>"
  VInt n      -> integer n
  VFloat n    -> double n
  VChar c     -> quotes $ prettyChar c
  VTuple vts  -> parens . hjoin ", " $ map (prettyVal LowP ctrs) vts
  VCtr l sp   -> prettyVCtr p ctrs (ctrs ! l) sp
  VIO{}       -> text "<io action>"

-- | Convert a term value to a pretty string.
--
-- @since 1.0.0
showValPrec :: Prec -> CtrNames -> Val -> String
showValPrec p ctrs vt = render $ prettyVal p ctrs vt

-- | Same as 'showValPrec LowP'.
--
-- @since 1.0.0
showVal :: CtrNames -> Val -> String
showVal = showValPrec LowP
