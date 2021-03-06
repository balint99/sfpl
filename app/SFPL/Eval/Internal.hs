{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TupleSections #-}

-- | Internal logic for evaluation.
module SFPL.Eval.Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IArray ((!))
import Data.Bits
import Data.Char (ord, chr)
import Data.List (intercalate)
import SFPL.Base
import SFPL.Elab.Metacontext
import SFPL.Eval.Types
import SFPL.Eval.Instances
import SFPL.Syntax.Core
import SFPL.Utils
import Text.PrettyPrint (Doc)

----------------------------------------
-- Operator precedence

infixl 9 $$$
infixl 9 $$
infixl 5 +>
infixl 5 |>

------------------------------------------------------------
-- Types

-- | Type closure application.
--
-- @since 1.0.0
($$$) :: TClosure -> VTy -> EvalT VTy
TClosure env a $$$ vb = evalTy (env :> vb) a

-- | Apply a metavariable solution to actual parameters.
applyMeta :: Ty -> VTSpine -> EvalT VTy
applyMeta a sp = evalTy sp a

-- | Evaluate a metavariable.
evalMeta :: Metavar -> VTSpine -> EvalT VTy
evalMeta m sp = do
  (metaSt, _) <- getMeta m <$> ask
  case metaSt of
    Solved a  -> applyMeta a sp
    Unsolved  -> pure $ VMeta m sp

-- | Evaluate a type in the given environment.
--
-- @since 1.0.0
evalTy :: TEnv -> Ty -> EvalT VTy
evalTy env = \case
  TyVar i     -> pure $ env !! unIx i
  Data l sp   -> VData l <$> evalTSp env sp
  Meta m sp   -> evalMeta m =<< evalTSp env sp
  FreshMeta m -> evalMeta m env
  Int         -> pure VTInt
  Float       -> pure VTFloat
  Char        -> pure VTChar
  Tuple as    -> VTTuple <$> (evalTy env <$$> as)
  World a     -> VWorld <$> evalTy env a
  Fun a b     -> VFun <$> evalTy env a <*> evalTy env b
  ForAll x a  -> pure $ VForAll x (TClosure env a)
  THole       -> devError "tried to evaluate type hole"

-- | Evaluate a type spine in the given environment.
evalTSp :: TEnv -> TSpine -> EvalT VTSpine
evalTSp env = \case
  []      -> pure []
  sp :> a -> (:>) <$> evalTSp env sp <*> evalTy env a

-- | Forcing of metavariables.
--
-- @since 1.0.0
forceTy :: VTy -> EvalT VTy
forceTy = \case
  va@(VMeta m sp) -> do (metaSt, _) <- getMeta m <$> ask
                        case metaSt of
                          Solved a  -> forceTy =<< applyMeta a sp
                          Unsolved  -> pure va
  va              -> pure va

-- | Quote a type value.
--
-- @since 1.0.0
quoteTy :: Lvl -> VTy -> EvalT Ty
quoteTy n va = forceTy va >>= \case
  VTyVar l    -> pure $ TyVar (lvl2Ix n l)
  VData l sp  -> Data l <$> quoteTSp n sp
  VMeta m sp  -> Meta m <$> quoteTSp n sp
  VTInt       -> pure Int
  VTFloat     -> pure Float
  VTChar      -> pure Char
  VTTuple vas -> Tuple <$> (quoteTy n <$$> vas)
  VWorld va   -> World <$> quoteTy n va
  VFun va vb  -> Fun <$> quoteTy n va <*> quoteTy n vb
  VForAll x b -> ForAll x <$> (quoteTy (n + 1) =<< b $$$ VTyVar n)

-- | Quote a type value spine.
quoteTSp :: Lvl -> VTSpine -> EvalT TSpine
quoteTSp n = \case
  []        -> pure []
  sp :> va  -> (:>) <$> quoteTSp n sp <*> quoteTy n va
    
normalizeMeta :: Lvl -> TEnv -> Metavar -> EvalT Ty
normalizeMeta n env m = undefined

-- | Normalize a type.
normalizeTy :: Lvl -> TEnv -> Ty -> EvalT Ty
normalizeTy n env = \case
  Data l sp     -> Data l <$> normalizeTSp n env sp
  a@Meta{}      -> quoteTy n =<< evalTy env a
  a@FreshMeta{} -> quoteTy n =<< evalTy env a
  Tuple as      -> Tuple <$> (normalizeTy n env <$$> as)
  World a       -> World <$> normalizeTy n env a
  Fun a b       -> Fun <$> normalizeTy n env a <*> normalizeTy n env b
  ForAll x a    -> ForAll x <$> normalizeTy (n + 1) (env :> VTyVar n) a
  a             -> pure a

-- | Normalize a type spine.
normalizeTSp :: Lvl -> TEnv -> TSpine -> EvalT TSpine
normalizeTSp n env = \case
  []      -> pure []
  sp :> a -> (:>) <$> normalizeTSp n env sp <*> normalizeTy n env a

-- | Normalize the types in a term.
normalizeTm :: Lvl -> TEnv -> Tm -> EvalT Tm
normalizeTm n env = \case
  Lam x a t     -> Lam x <$> normalizeTy n env a <*> normalizeTm n env t
  LamI x t      -> LamI x <$> normalizeTm (n + 1) (env :> VTyVar n) t
  App t u       -> App <$> normalizeTm n env t <*> normalizeTm n env u
  AppI t a      -> AppI <$> normalizeTm n env t <*> normalizeTy n env a
  Let x a t u   -> Let x <$> normalizeTy n env a <*> normalizeTm n env t <*> normalizeTm n env u
  Tup ts        -> Tup <$> (normalizeTm n env <$$> ts)
  UnOp op t     -> UnOp op <$> normalizeTm n env t
  BinOp op t u  -> BinOp op <$> normalizeTm n env t <*> normalizeTm n env u
  UnFunc f t    -> UnFunc f <$> normalizeTm n env t
  BinFunc f t u -> BinFunc f <$> normalizeTm n env t <*> normalizeTm n env u
  Case t bs     -> Case <$> normalizeTm n env t <*> (normalizeCaseBranch n env <$$> bs)
  Bind x a t u  -> Bind x <$> normalizeTy n env a <*> normalizeTm n env t <*> normalizeTm n env u
  t             -> pure t
  where
    normalizeCaseBranch n env (p, t) = case p of
      PCtr l args -> (p, ) <$> normalizeWithArgs args n env t
      _           -> (p, ) <$> normalizeTm n env t
    normalizeWithArgs args n env t = case args of
      Right x : args  -> normalizeWithArgs args (n + 1) (env :> VTyVar n) t
      _               -> normalizeTm n env t

-- | Normalize the types in the given program by substituting the solutions
-- of metavariables using the given metavariable mappings.
--
-- @since 1.0.0
normalizeTypes :: Program -> EvalT Program
normalizeTypes = \case
  []      -> pure []
  d : ds  -> (:) <$> normalizeDecl d <*> normalizeTypes ds
  where
    normalizeDecl = \case
      Left td   -> pure $ Left td
      Right tl  -> Right <$> normalizeTopLevelDef tl
    normalizeTopLevelDef (TL l a t) = TL l <$> normalizeTy 0 [] a <*> normalizeTm 0 [] t

-- | Information context for type values.
--
-- @since 1.0.0
type VTyPCxt = (TyPCxt, SomeMetas)

-- | Pretty-print a type value, using the given information context.
--
-- @since 1.0.0
prettyVTy :: Prec -> VTyPCxt -> VTy -> Doc
prettyVTy p (tcxt@(xs, _, _), metas) va =
  prettyPrec p tcxt $ quoteTy (Lvl $ length xs) va metas

-- | @since 1.0.0
instance Pretty VTyPCxt VTy where
  prettyPrec = prettyVTy

------------------------------------------------------------
-- Terms

-- Convert a value to a string.
valToString :: Val -> EvalP String
valToString vt = ask >>= \(_, ctrs) -> pure $ showPretty ctrs vt

-- Helper for evaluation errors.
devEvalError :: String -> [Val] -> EvalP a
devEvalError s vts = do
  xs <- valToString <$$> vts
  devError $ s ++ intercalate ", " xs

-- | Extend an environment with multiple values.
(+>) :: Env -> [Val] -> Env
env +> []       = env
env +> (v : vs) = (env :> v) +> vs 

-- | Extend an environment with a spine.
(|>) :: Env -> VSpine -> Env
(|>) = flip (++)

-- | Closure application.
--
-- @since 1.0.0
($$) :: Closure -> Val -> EvalP Val
Closure env t $$ vu = eval (env :> vu) t

-- | Value application.
vApp :: Val -> Val -> EvalP Val
vApp vt vu = case vt of
  VLam cl   -> cl $$ vu
  VCtr l sp -> pure $ VCtr l (sp :> vu)
  _         -> devEvalError "tried to apply " [vt]

evalUnOp :: Env -> UnaryOp -> Tm -> EvalP Val
evalUnOp env Pure t = pure . VIO $ VPure env t
evalUnOp env op   t = do
  vt <- eval env t
  case op of
    Negate  -> case vt of
      VInt n    -> pure $ VInt (-n)
      VFloat n  -> pure $ VFloat (-n)
      _         -> devEvalError "can't negate " [vt]
    BNot    -> case vt of
      VInt n    -> pure $ VInt (complement n)
      _         -> devEvalError "can't take bitwise complement of " [vt]

evalBinOp :: Env -> BinaryOp -> Tm -> Tm -> EvalP Val
evalBinOp env op t u = do
  vt <- eval env t
  vu <- eval env u
  case op of
    Add   -> case (vt, vu) of
      (VInt n,   VInt n'  ) -> pure $ VInt (n + n')
      (VFloat n, VFloat n') -> pure $ VFloat (n + n')
      _                     -> devEvalError "can't add: " [vt, vu]
    Sub   -> case (vt, vu) of
      (VInt n,   VInt n'  ) -> pure $ VInt (n - n')
      (VFloat n, VFloat n') -> pure $ VFloat (n - n')
      _                     -> devEvalError "can't subtract: " [vt, vu]
    Mul   -> case (vt, vu) of
      (VInt n,   VInt n'  ) -> pure $ VInt (n * n')
      (VFloat n, VFloat n') -> pure $ VFloat (n * n')
      _                     -> devEvalError "can't multiply: " [vt, vu]
    Div   -> case (vt, vu) of
      (VInt n,   VInt n'  ) | n' == 0   -> throwError "divide by zero"
                            | otherwise -> pure $ VInt (n `div` n')
      (VFloat n, VFloat n') | n' == 0   -> throwError "divide by zero"
                            | otherwise -> pure $ VFloat (n / n')
      _                     -> devEvalError "can't divide: " [vt, vu]
    Exp   -> case (vt, vu) of
      (VInt n,   VInt n'  ) | n' < 0    -> throwError "negative exponent"
                            | otherwise -> pure $ VInt (n ^ n')
      (VFloat n, VFloat n') | n == 0 && n' < 0  -> throwError "invalid arguments to ^"
                            | otherwise         -> pure $ VFloat (n ** n')
      _                     -> devEvalError "can't exponentiate: " [vt, vu]
    BAnd  -> case (vt, vu) of
      (VInt n,   VInt n'  ) -> pure $ VInt (n .&. n')
      _                     -> devEvalError "can't take bitwise and: " [vt, vu]
    BOr   -> case (vt, vu) of
      (VInt n,   VInt n'  ) -> pure $ VInt (n .|. n')
      _                     -> devEvalError "can't take bitwise or: " [vt, vu]

evalNullFunc :: NullaryFunc -> EvalP Val
evalNullFunc = pure . VIO . \case
  Read  -> VRead
  Peek  -> VPeek
  IsEOF -> VIsEOF

evalUnFunc :: Env -> UnaryFunc -> Tm -> EvalP Val
evalUnFunc env Putc  t = pure . VIO $ VPutc env t
evalUnFunc env Print t = pure . VIO $ VPrint env t
evalUnFunc env f     t = do
  vt <- eval env t
  case f of
    ToInt   -> case vt of
      VInt n    -> pure $ VInt n
      VFloat n  -> pure $ VInt (floor n)
      VChar n   -> pure $ VInt (toInteger $ ord n)
      _         -> devEvalError "can't convert to integer: " [vt]
    ToFloat -> case vt of
      VInt n    -> pure $ VFloat (fromInteger n)
      VFloat n  -> pure $ VFloat n
      _         -> devEvalError "can't convert to floating point number: " [vt]
    ToChar  -> case vt of
      VInt n    -> pure $ VChar (chr $ fromInteger n)
      VChar c   -> pure $ VChar c
      _         -> devEvalError "can't convert to character: " [vt]
    Error   -> throwError =<< valToString vt

boolToInt :: Bool -> Integer
boolToInt b = if b then 1 else 0

evalBinFunc :: Env -> BinaryFunc -> Tm -> Tm -> EvalP Val
evalBinFunc env f t u = do
  vt <- eval env t
  vu <- eval env u
  case f of
    PrimEq  -> case (vt, vu) of
      (VInt n  , VInt n'  ) -> pure $ VInt (boolToInt $ n == n')
      (VFloat n, VFloat n') -> pure $ VInt (boolToInt $ n == n')
      (VChar c , VChar c' ) -> pure $ VInt (boolToInt $ c == c')
      _                     -> devEvalError "can't compare: " [vt, vu]
    PrimLt  -> case (vt, vu) of
      (VInt n  , VInt n'  ) -> pure $ VInt (boolToInt $ n < n')
      (VFloat n, VFloat n') -> pure $ VInt (boolToInt $ n < n')
      (VChar c , VChar c' ) -> pure $ VInt (boolToInt $ c < c')
      _                     -> devEvalError "can't compare: " [vt, vu]

caseSplit :: Env -> Val -> [CaseBranch] -> EvalP Val
caseSplit env vt = \case
  []      -> throwError "non-exhaustive patterns"
  b : bs  -> let (p, t) = b in case (vt, p) of
    (VInt n     , PInt n'   ) | n == n'  -> eval env t
    (VFloat n   , PFloat n' ) | n == n'  -> eval env t
    (VChar c    , PChar c'  ) | c == c'  -> eval env t
    (VTuple vts , PTuple xs )            -> eval (env +> vts) t
    (VCtr l sp  , PCtr l' xs) | l == l'  -> eval (env |> sp) t
    (_          , PWildcard )            -> eval env t
    _                                    -> caseSplit env vt bs

-- | Pure evaluation of terms. Does not perform IO actions.
-- Precondition: the term is well-typed.
--
-- @since 1.0.0
eval :: Env -> Tm -> EvalP Val
eval env = \case
  Var i         -> pure $ env !! unIx i
  TopLevel l    -> ask >>= \(tl, _) -> eval env (tl ! l)
  Lam x a t     -> pure $ VLam (Closure env t)
  LamI x t      -> eval env t
  App t u       -> bindM2 vApp (eval env t) (eval env u)
  AppI t a      -> eval env t
  Let x a t u   -> eval env t >>= \vt -> eval (env :> vt) u
  IntLit n      -> pure $ VInt n
  FloatLit n    -> pure $ VFloat n
  CharLit c     -> pure $ VChar c
  Tup ts        -> VTuple <$> (eval env <$$> ts)
  Ctr l         -> pure $ VCtr l []
  UnOp op t     -> evalUnOp env op t
  BinOp op t u  -> evalBinOp env op t u
  NullFunc f    -> evalNullFunc f
  UnFunc f t    -> evalUnFunc env f t
  BinFunc f t u -> evalBinFunc env f t u
  Case t bs     -> eval env t >>= \vt -> caseSplit env vt bs
  Bind x a t u  -> pure . VIO $ VBind env t u
  Hole          -> devError "tried to evaluate hole"

----------------------------------------
-- Effects

-- Helpers

getTopLevel :: MonadRun m => m TopLevel
getTopLevel = (\(tl, _) -> tl) <$> getEvalCxt

getCtrNames :: MonadRun m => m CtrNames
getCtrNames = (\(_, ctrs) -> ctrs) <$> getEvalCxt

-- | Lift a pure evaluation to an effectful evaluation.
liftPure :: MonadRun m => EvalP a -> m a
liftPure p = do
  cxt <- getEvalCxt
  case runReaderT p cxt of
    Left e  -> throwEvalError e
    Right a -> pure a

valToStringP :: MonadRun m => Val -> m String
valToStringP = liftPure . valToString

evalP :: MonadRun m => Env -> Tm -> m Val
evalP env = liftPure . eval env

-- Helper for evaluation errors.
devRunError :: MonadRun m => String -> [Val] -> m a
devRunError s = liftPure . devEvalError s

returnChar :: MonadRun m => Maybe Char -> m Val
returnChar = maybe (throwEvalError "reached end of input") (pure . VChar)

forcePutc :: MonadRun m => Env -> Tm -> m Val
forcePutc env t = do
  vt <- evalP env t
  case vt of
    VChar c -> VTt <$ write [c]
    _       -> devRunError "can't put " [vt]

forcePrint :: MonadRun m => Env -> Tm -> m Val
forcePrint env t = VTt <$ (write =<< valToStringP =<< evalP env t)

forceIOVal :: MonadRun m => IOVal -> m Val
forceIOVal = \case
  VPure env t   -> evalP env t
  VBind env t u -> run env t >>= \vt -> run (env :> vt) u
  VRead         -> readChar >>= returnChar
  VPeek         -> peekChar >>= returnChar
  VIsEOF        -> VInt . maybe 1 (const 0) <$> peekChar
  VPutc env t   -> forcePutc env t
  VPrint env t  -> forcePrint env t

-- | Perform the outermost layer of IO actions in a value.
forceIO :: MonadRun m => Val -> m Val
forceIO = \case
  VIO v -> forceIOVal v
  vt    -> devRunError "tried to force " [vt]

-- | Evaluate a term with effects.
--
-- @since 1.0.0
run :: MonadRun m => Env -> Tm -> m Val
run env = forceIO <=< evalP env
