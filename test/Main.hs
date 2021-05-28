
-- | Main program of tests.
module Main where

import SFPL.Parser
import Test.Hspec

import qualified SFPL.ElabSpec as Elab
import qualified SFPL.EvalSpec as Eval
import qualified SFPL.ParserSpec as Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"       Parser.spec
  describe "Elaboration"  Elab.spec
  describe "Evaluation"   Eval.spec
