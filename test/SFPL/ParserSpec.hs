{-# LANGUAGE ViewPatterns #-}

-- | Tests for the parser.
module SFPL.ParserSpec
  ( spec )
  where

import SFPL.Base
import SFPL.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

prefix :: FilePath
prefix = "test/file/parser/"

actualPath :: FilePath -> FilePath -> FilePath
actualPath group path = prefix ++ group ++ "/" ++ path ++ ".txt"

shouldSucceed :: (Eq a, Show a) => Parser a -> FilePath -> Expectation
shouldSucceed p (actualPath "success" -> file) = do
  src <- readFile file
  let res = parse (topLevel p) file src
  res `parseSatisfies` ((== src) . show)

shouldFail :: Show a => Parser a -> FilePath -> Expectation
shouldFail p (actualPath "failure" -> file) = do
  src <- readFile file
  let p' = parse (topLevel p) file
  p' `shouldFailOn` src

spec :: Spec
spec = do
  describe "success" $ do
    describe "types" $ do
      it "1" $ ty `shouldSucceed` "ty1"
      it "2" $ ty `shouldSucceed` "ty2"
      it "3" $ ty `shouldSucceed` "ty3"
      it "4" $ ty `shouldSucceed` "ty4"
      it "5" $ ty `shouldSucceed` "ty5"
    
    describe "patterns" $ do
      it "1"  $ pat `shouldSucceed` "pat1"
      it "2"  $ pat `shouldSucceed` "pat2"
      it "3"  $ pat `shouldSucceed` "pat3"
      it "4"  $ pat `shouldSucceed` "pat4"
      it "5"  $ pat `shouldSucceed` "pat5"
      it "6"  $ pat `shouldSucceed` "pat6"
      it "7"  $ pat `shouldSucceed` "pat7"
      it "8"  $ pat `shouldSucceed` "pat8"
      it "9"  $ pat `shouldSucceed` "pat9"
      it "10" $ pat `shouldSucceed` "pat10"
    
    describe "terms" $ do
      it "1" $ tm `shouldSucceed` "tm1"
      it "2" $ tm `shouldSucceed` "tm2"
      it "3" $ tm `shouldSucceed` "tm3"
      it "4" $ tm `shouldSucceed` "tm4"
      it "5" $ tm `shouldSucceed` "tm5"
      it "6" $ tm `shouldSucceed` "tm6"
      it "7" $ tm `shouldSucceed` "tm7"
      it "8" $ tm `shouldSucceed` "tm8"
      it "9" $ tm `shouldSucceed` "tm9"
    
    describe "data types" $ do
      it "1" $ dataDecl `shouldSucceed` "dd1"
      it "2" $ dataDecl `shouldSucceed` "dd2"
      it "3" $ dataDecl `shouldSucceed` "dd3"
    
    describe "programs" $ do
      it "1" $ program `shouldSucceed` "prog1"
    
  describe "failure" $ do
    describe "types" $ do
      it "1" $ ty `shouldFail` "ty1"
      it "2" $ ty `shouldFail` "ty2"
      it "3" $ ty `shouldFail` "ty3"
      it "4" $ ty `shouldFail` "ty4"
      it "5" $ ty `shouldFail` "ty5"
      it "6" $ ty `shouldFail` "ty6"

    describe "patterns" $ do
      it "1" $ pat `shouldFail` "pat1"
      it "2" $ pat `shouldFail` "pat2"
      it "3" $ pat `shouldFail` "pat3"
      it "4" $ pat `shouldFail` "pat4"
      it "5" $ pat `shouldFail` "pat5"
      it "6" $ pat `shouldFail` "pat6"
      
    describe "terms" $ do
      it "1" $ tm `shouldFail` "tm1"
      it "2" $ tm `shouldFail` "tm2"
      it "3" $ tm `shouldFail` "tm3"
      it "4" $ tm `shouldFail` "tm4"
      it "5" $ tm `shouldFail` "tm5"
      it "6" $ tm `shouldFail` "tm6"
      it "6" $ tm `shouldFail` "tm6"
      it "7" $ tm `shouldFail` "tm7"
      it "8" $ tm `shouldFail` "tm8"
    
    describe "data types" $ do
      it "1" $ dataDecl `shouldFail` "dd1"
      it "2" $ dataDecl `shouldFail` "dd2"
      it "3" $ dataDecl `shouldFail` "dd3"
      
    describe "programs" $ do
      it "1" $ program `shouldFail` "prog1"
