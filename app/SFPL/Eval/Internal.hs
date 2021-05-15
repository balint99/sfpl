{-# LANGUAGE LambdaCase #-}

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

----------------------------------------
-- Operator precedence

infixl 9 $$$
infixl 9 $$
infixl 5 +>
infixl 5 |>

------------------------------------------------------------
-- Types

-- | Type closure application.
($$$) :: MonadMeta m => TClosure -> VTy -> m VTy
TClosure env a $$$ vb = evalTy (env :> vb) a

-- | Apply a metavariable solution to actual parameters.
applyMeta :: MonadMeta m => Ty -> VTSpine -> m VTy
applyMeta a sp = evalTy sp a

-- | Evaluate a metavariable.
evalMeta :: MonadMeta m => Metavar -> VTSpine -> m VTy
evalMeta m sp = do
  entry <- lookupMeta m
  case entry of
    Solved a  -> applyMeta a sp
    Unsolved  -> pure $ VMeta m sp

-- | Evaluate a type in the given environment.
--
-- @since 1.0.0
evalTy :: MonadMeta m => TEnv -> Ty -> m VTy
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

-- | Evaluate a type spine in the given environment.
evalTSp :: MonadMeta m => TEnv -> TSpine -> m VTSpine
evalTSp env = \case
  []      -> pure []
  sp :> a -> (:>) <$> evalTSp env sp <*> evalTy env a

-- | Convert de-Bruijn level to index.
lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl n) (Lvl l) = Ix (n - l - 1)

-- | Quote a type value.
--
-- @since 1.0.0
quoteTy :: MonadMeta m => Lvl -> VTy -> m Ty
quoteTy n = \case
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
quoteTSp :: MonadMeta m => Lvl -> VTSpine -> m TSpine
quoteTSp n = \case
  []      -> pure []
  sp :> a -> (:>) <$> quoteTSp n sp <*> quoteTy n a

-- | Show a type value using the given information context.
--
-- @since 1.0.0
showVTy :: MonadMeta m => TyPCxt -> VTy -> m String
showVTy cxt@(xs, _) va = showPretty cxt <$> quoteTy (Lvl $ length xs) va

-- | Forcing of metavariables.
--
-- @since 1.0.0
forceTy :: MonadMeta m => VTy -> m VTy
forceTy = \case
  va@(VMeta m sp) -> do entry <- lookupMeta m
                        case entry of
                          Solved a  -> forceTy =<< applyMeta a sp
                          Unsolved  -> pure va
  va              -> pure va

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

----------------------------------------
-- Effects

getTopLevel :: MonadRun m => m TopLevel
getTopLevel = (\(tl, _) -> tl) <$> getEvalCxt

getCtrNames :: MonadRun m => m CtrNames
getCtrNames = (\(_, ctrs) -> ctrs) <$> getEvalCxt

-- | Lift a pure evaluation to an effectful evaluation.
liftPure :: MonadRun m => EvalP a -> m a
liftPure p = do
  cxt <- getEvalCxt
  case runReaderT p cxt of
    Left e  -> throwErr e
    Right a -> pure a

valToStringP :: MonadRun m => Val -> m String
valToStringP = liftPure . valToString

evalP :: MonadRun m => Env -> Tm -> m Val
evalP env = liftPure . eval env

-- Helper for evaluation errors.
devRunError :: MonadRun m => String -> [Val] -> m a
devRunError s = liftPure . devEvalError s

returnChar :: MonadRun m => Maybe Char -> m Val
returnChar = maybe (throwErr "reached end of input") (pure . VChar)

forcePutc :: MonadRun m => Env -> Tm -> m Val
forcePutc env t = do
  vt <- evalP env t
  case vt of
    VChar c -> VUnit <$ write [c]
    _       -> devRunError "can't put " [vt]

forcePrint :: MonadRun m => Env -> Tm -> m Val
forcePrint env t = VUnit <$ (write =<< valToStringP =<< evalP env t)

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
run :: MonadRun m => Env -> Tm -> m Val
run env = forceIO <=< evalP env
