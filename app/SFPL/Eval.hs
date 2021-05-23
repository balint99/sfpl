{-# LANGUAGE FlexibleInstances, PatternSynonyms #-}

-- | Type and term evaluation.
module SFPL.Eval
  ( -- * Types
    VTy (.., VUnit),
    VTSpine,
    TEnv,
    TClosure (..),
    
    -- * Terms
    IOVal (..),
    Val (.., VTt),
    VSpine,
    Env,
    Closure (..),
    
    -- * Other definitions
    TopLevel,
    CtrNames,
    EvalCxt,
    EvalError,
    EvalT,
    EvalP,
    Eval,
    
    -- * Effectful evaluation monad class
    MonadRun (..),
    
    -- * Functions
    evalCxt,
    emptyEvalCxt,
    
    -- ** Types
    ($$$),
    evalTy,
    forceTy,
    quoteTy,
    VTyPCxt,
    prettyVTy,
    
    -- ** Terms
    ($$),
    eval,
    evalClosed,
    run,
    runClosed,
    runMain,
  )
  where

import SFPL.Eval.Types
import SFPL.Eval.Pretty
import SFPL.Eval.Instances
import SFPL.Eval.Internal

import Control.Monad.Reader
import SFPL.Base
import SFPL.Syntax.Core
import SFPL.Utils
import System.IO

----------------------------------------
-- Helpers

-- | Create an evaluation context from the given list of
-- top-level definitions and data constructor names.
--
-- @since 1.0.0
evalCxt :: [Tm] -> [Name] -> EvalCxt
evalCxt ts xs = (arr ts, arr xs)

-- | The empty evaluation context.
--
-- @since 1.0.0
emptyEvalCxt :: EvalCxt
emptyEvalCxt = evalCxt [] []

-- | Evaluate a closed term without performing effects using
-- the given evaluation context. Returns the result of
-- evaluation, which can be also be an error.
--
-- @since 1.0.0
evalClosed :: EvalCxt -> Tm -> Either EvalError Val
evalClosed cxt t = runReaderT (eval [] t) cxt

----------------------------------------
-- Concrete evaluation monad

-- | Effectful evaluation monad.
--
-- @since 1.0.0
type Eval = ReaderT EvalCxt IO

-- | Read the next character from standard input.
-- Returns 'Nothing' if end of file has been reached;
-- otherwise returns the next character in a 'Just'.
readCharEval :: Eval (Maybe Char)
readCharEval = do
  b <- liftIO isEOF
  if b
    then pure Nothing
    else Just <$> liftIO getChar

-- | Get the next character from standard input without
-- actually consuming it. Returns 'Nothing' if end of file
-- has been reached; otherwise returns the next character in a 'Just'.
peekCharEval :: Eval (Maybe Char)
peekCharEval =  do
  b <- liftIO isEOF
  if b
    then pure Nothing
    else Just <$> liftIO (hLookAhead stdin)

-- | @since 1.0.0
instance MonadRun Eval where
  getEvalCxt = ask
  throwEvalError = error
  readChar = readCharEval
  peekChar = peekCharEval
  write = liftIO . putStr

-- | Evaluate a closed term with side effects using
-- the given evaluation context. Returns the result of evaluation
--
-- @since 1.0.0
runClosed :: EvalCxt -> Tm -> IO Val
runClosed cxt t = runReaderT (run [] t) cxt

-- | Run the main function given its top-level identifier,
-- and discard the result of evaluation.
--
-- @since 1.0.0
runMain :: EvalCxt -> Lvl -> IO ()
runMain cxt l = () <$ runClosed cxt (TopLevel l)
