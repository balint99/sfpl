{-# LANGUAGE FlexibleInstances, OverloadedLists, OverloadedStrings, TypeFamilies #-}

-- | Evaluation of types and terms.
module SFPL.Eval
  ( module SFPL.Eval.Types,
    module SFPL.Eval.Internal,
    module SFPL.Eval.Pretty,
    module SFPL.Eval,
  )
  where

import SFPL.Base
import SFPL.Syntax.Core
import Control.Monad.Reader
import SFPL.Utils
import System.IO
import GHC.Exts

import SFPL.Eval.Types
import SFPL.Eval.Internal
import SFPL.Eval.Pretty
import SFPL.Eval.Instances

----------------------------------------
-- Evaluation

-- | Evaluate a closed term without performing effects using
-- the given top-level definitions. Evaluation can fail with an error.
--
-- @since 1.0.0
evalTm :: EvalCxt -> Tm -> Either EvalError Val
evalTm cxt t = runReaderT (eval [] t) cxt

evalTest :: EvalCxt -> Tm -> IO ()
evalTest cxt@(_, ctrs) t =
  putStrLn . either prettyError (showVal ctrs) $ evalTm cxt t
  where
    prettyError s = "\nA runtime error occurred: " ++ s ++ "\n"

type Eval = ReaderT EvalCxt IO

instance MonadRun Eval where
  getEvalCxt = ask
  
  throwErr = error
  
  readChar = do
    b <- liftIO isEOF
    if b
      then pure Nothing
      else Just <$> liftIO getChar
  
  peekChar = do
    b <- liftIO isEOF
    if b
      then pure Nothing
      else Just <$> liftIO (hLookAhead stdin)
  
  write = liftIO . putStr

emptyCxt :: EvalCxt
emptyCxt = (arr [], arr [])

baseTypesCxt :: EvalCxt
baseTypesCxt = (arr [], arr ["true", "false", "nil", "cons", "nothing", "just"])

instance Num Tm where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UnOp Negate
  fromInteger = IntLit
  abs = undefined
  signum = undefined

instance Num Pattern where
  fromInteger = PInt
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined

instance Fractional Tm where
  (/) = BinOp Div
  fromRational = FloatLit . fromRational

instance Fractional Pattern where
  fromRational = PFloat . fromRational
  (/) = undefined

true = Ctr 0; trueP = PCtr 0 []
false = Ctr 1; falseP = PCtr 1 []
nil = Ctr 2; nilP = PCtr 2 []
x <| y = App (App (Ctr 3) x) y; infixr 5 <|
x <:: y = PCtr 3 [Left x, Left y]; infixr 5 <::
__ = PWildcard

instance IsList Tm where
  type Item Tm = Tm
  
  fromList = foldr (<|) nil
  toList = undefined

instance IsList Pattern where
  type Item Pattern = Name
  
  fromList = PTuple
  toList = undefined

instance IsString Tm where
  fromString = fromList . map CharLit

lam = Lam "_" undefined
($|) = App; infixl 9 $|
(>>-) = Bind "_" undefined; infixr 2 >>-
(>>>) = Bind "_" undefined; infixl 1 >>>
ret = UnOp Pure

(.&.) = BinOp BAnd; infixl 7 .&.
(.|.) = BinOp BOr; infixl 6 .|.
bnot = UnOp BNot

nothing = Ctr 4
just x = App (Ctr 5) x

tt = Tup []

c = CharLit
read' = NullFunc Read
peek = NullFunc Peek
iseof = NullFunc IsEOF >>- Case (Var 0) [(1, ret true), (0, ret false)]
int = UnFunc ToInt
float = UnFunc ToFloat
char = UnFunc ToChar
err = UnFunc Error
putc = UnFunc Putc
print' = UnFunc Print
not' x = Case x [(trueP, false), (falseP, true)]
eq = BinFunc PrimEq
neq x y = not' (eq x y)
lt = BinFunc PrimLt

compose = lam $ lam $ lam $ Var 2 $| (Var 1 $| Var 0)

map' = lam $ lam $ Case (Var 0)
  [ (nilP, nil)
  , ("x" <:: "xs", Var 3 $| Var 1 <| map' $| Var 3 $| Var 0)
  ]

seq' = lam $ Case (Var 0)
  [ (nilP, ret nil)
  , ("x" <:: "xs", Var 1 >>- seq' $| Var 1 >>- ret (Var 1 <| Var 0))
  ]

seq_ = lam $ Case (Var 0)
  [ (nilP, ret tt)
  , ("x" <:: "xs", Var 1 >>> seq_ $| Var 1)
  ]

replicate' = lam $ lam $ Case (Var 1)
  [ (0 , nil)
  , (__, Var 0 <| replicate' $| (Var 1 - 1) $| Var 0)
  ]

puts = compose $| seq_ $| (map' $| lam (putc $ Var 0))
putsln = lam $ puts $| Var 0 >>> putc (c '\n')
printLn = lam (print' (Var 0) >>> putc (CharLit '\n'))
readStdIn = iseof >>- Case (Var 0)
  [ (trueP, ret nil)
  , (falseP, read' >>- readStdIn >>- ret (Var 1 <| Var 0))
  ]

res t = t >>- printLn $| Var 0


readLine = read' >>- Case (Var 0)
  [ (PChar '\n', ret nil)
  , (__, readLine >>- ret (Var 1 <| Var 0))
  ]

fact = lam $ Case (Var 0)
  [ (0 , 1)
  , (__, Var 0 * fact $| (Var 0 - 1))
  ]

reverse' = revHelper $| []
  where
    revHelper = lam $ lam $ Case (Var 0)
      [ (nilP, Var 1)
      , ("x" <:: "xs", revHelper $| (Var 1 <| Var 3) $| Var 0)
      ]

digitToInt = lam $ int (Var 0) - int (CharLit '0')

parseInt = parseInt' $| 0
  where
    parseInt' = lam $ lam $ Case (Var 0)
      [ (nilP, Var 1)
      , ("c" <:: "cs", parseInt' $| (10 * Var 3 + digitToInt $| Var 1) $| Var 0)
      ]

readInt = readLine >>- ret (parseInt $| Var 0)

myProg =
  putsln $| "Hello!" >>>
  putsln $| "Please enter a number: " >>>
  readInt >>- (
  putc (c '\n') >>>
  puts $| "The factorial of " >>> print' (Var 1) >>> puts $| " is: " >>> printLn $| (fact $| Var 1))
  

cxt1 :: EvalCxt
cxt1 = ( arr [iseof, printLn, readStdIn, fact]
       , arr ["true", "false", "nil", "cons", "nothing", "just"])

runTm :: EvalCxt -> Tm -> IO ()
runTm cxt t = runReaderT (() <$ run [] (t >>- printLn $| Var 0):: Eval ()) cxt
