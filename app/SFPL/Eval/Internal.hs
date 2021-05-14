{-# LANGUAGE LambdaCase #-}

module SFPL.Eval.Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IArray ((!))
import Data.Bits
import Data.Char (ord, chr)
import SFPL.Base
import SFPL.Elab.Metacontext
import SFPL.Eval.Types
import SFPL.Eval.Pretty
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
showVTy cxt@(xs, _) va = showTy cxt <$> quoteTy (Lvl $ length xs) va

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
  _         -> devError $ "can only apply to lambda or constructor"

evalUnOp :: Env -> UnaryOp -> Tm -> EvalP Val
evalUnOp env Pure t = pure . VIO $ VPure env t
evalUnOp env op t = do
  vt <- eval env t
  case op of
    Negate  -> pure $ case vt of
      VInt n    -> VInt (-n)
      VFloat n  -> VFloat (-n)
      _         -> devError "can't negate"
    BNot    -> pure $ case vt of
      VInt n    -> VInt (complement n)
      _         -> devError "can't take bitwise complement"

evalBinOp :: Env -> BinaryOp -> Tm -> Tm -> EvalP Val
evalBinOp env op t u = do
  vt <- eval env t
  vu <- eval env u
  case op of
    Add   -> pure $ case (vt, vu) of
      (VInt n,   VInt n'  ) -> VInt (n + n')
      (VFloat n, VFloat n') -> VFloat (n + n')
      _                     -> devError "can't add"
    Sub   -> pure $ case (vt, vu) of
      (VInt n,   VInt n'  ) -> VInt (n - n')
      (VFloat n, VFloat n') -> VFloat (n - n')
      _                     -> devError "can't subtract"
    Mul   -> pure $ case (vt, vu) of
      (VInt n,   VInt n'  ) -> VInt (n * n')
      (VFloat n, VFloat n') -> VFloat (n * n')
      _                     -> devError "can't multiply"
    Div   -> case (vt, vu) of
      (VInt n,   VInt n'  ) | n' == 0   -> throwError "divide by zero"
                            | otherwise -> pure $ VInt (n `div` n')
      (VFloat n, VFloat n') | n' == 0   -> throwError "divide by zero"
                            | otherwise -> pure $ VFloat (n / n')
      _                     -> devError "can't divide"
    Exp   -> case (vt, vu) of
      (VInt n,   VInt n'  ) | n' < 0    -> throwError "negative exponent"
                            | otherwise -> pure $ VInt (n ^ n')
      (VFloat n, VFloat n') | n == 0 && n' < 0  -> throwError "invalid arguments"
                            | otherwise         -> pure $ VFloat (n ** n')
      _                     -> devError "can't exponentiate"
    BAnd  -> pure $ case (vt, vu) of
      (VInt n,   VInt n'  ) -> VInt (n .&. n')
      _                     -> devError "can't take bitwise and"
    BOr   -> pure $ case (vt, vu) of
      (VInt n,   VInt n'  ) -> VInt (n .|. n')
      _                     -> devError "can't take bitwise or"

evalNullFunc :: NullaryFunc -> EvalP Val
evalNullFunc = pure . VIO . \case
  Read  -> VRead
  Peek  -> VPeek
  IsEOF -> VIsEOF

evalUnFunc :: Env -> UnaryFunc -> Tm -> EvalP Val
evalUnFunc env Putc  t = pure . VIO $ VPutc env t
evalUnFunc env Print t = pure . VIO $ VPrint env t
evalUnFunc env f t = do
  vt <- eval env t
  case f of
    ToInt   -> pure $ case vt of
      VInt n    -> VInt n
      VFloat n  -> VInt (floor n)
      VChar n   -> VInt (toInteger $ ord n)
      _         -> devError "can't convert to integer"
    ToFloat -> pure $ case vt of
      VInt n    -> VFloat (fromInteger n)
      VFloat n  -> VFloat n
      _         -> devError "can't convert to floating point number"
    ToChar  -> pure $ case vt of
      VInt n    -> VChar (chr $ fromInteger n)
      VChar c   -> VChar c
      _         -> devError "can't convert to character"
    Error   -> ask >>= \(_, ctrs) -> throwError (showVal ctrs vt)

boolToInt :: Bool -> Integer
boolToInt b = if b then 1 else 0

evalBinFunc :: Env -> BinaryFunc -> Tm -> Tm -> EvalP Val
evalBinFunc env f t u = do
  vt <- eval env t
  vu <- eval env u
  case f of
    PrimEq  -> pure $ case (vt, vu) of
      (VInt n  , VInt n'  ) -> VInt (boolToInt $ n == n')
      (VFloat n, VFloat n') -> VInt (boolToInt $ n == n')
      (VChar c , VChar c' ) -> VInt (boolToInt $ c == c')
      _                     -> devError "can't compare"
    PrimLt  -> pure $ case (vt, vu) of
      (VInt n  , VInt n'  ) -> VInt (boolToInt $ n < n')
      (VFloat n, VFloat n') -> VInt (boolToInt $ n < n')
      (VChar c , VChar c' ) -> VInt (boolToInt $ c < c')
      _                     -> devError "can't compare"

caseSplit :: Env -> Val -> [CaseBranch] -> EvalP Val
caseSplit env vt = \case
  []      -> ask >>= \(_, ctrs) -> throwError $ "non-exhaustive patterns: " ++ showVal ctrs vt
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

returnChar :: MonadRun m => Maybe Char -> m Val
returnChar = maybe (throwErr "reached end of input") (pure . VChar)

forcePutc :: MonadRun m => Env -> Tm -> m Val
forcePutc env t = do
  vt <- liftPure $ eval env t
  case vt of
    VChar c -> VUnit <$ write [c]
    _       -> devError "can't put non-character"

forcePrint :: MonadRun m => Env -> Tm -> m Val
forcePrint env t = do
  vt <- liftPure $ eval env t
  ctrs <- getCtrNames
  VUnit <$ write (showVal ctrs vt)

forceIOVal :: MonadRun m => IOVal -> m Val
forceIOVal = \case
  VPure env t   -> liftPure $ eval env t
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
  _     -> devError $ "tried to force non-IO value"

-- | Evaluate a term with effects.
run :: MonadRun m => Env -> Tm -> m Val
run env = forceIO <=< liftPure . eval env
