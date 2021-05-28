
-- | Tests for evaluation.
module SFPL.EvalSpec
  ( spec )
  where

import Test.Hspec

success :: Spec
success = pure ()

failure :: Spec
failure = pure ()

spec :: Spec
spec = do
  describe "success" success
  describe "failure" failure
