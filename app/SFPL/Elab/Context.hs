{-# LANGUAGE LambdaCase #-}

-- | The context of elaboration.
module SFPL.Elab.Context where

import Data.HashMap.Lazy (HashMap)
import SFPL.Base
import SFPL.Eval.Types
import SFPL.Syntax.Core.Types
import SFPL.Syntax.Raw.Types (BegPos)

-- | The top-level context at a given point of elaboration.
--
-- @since 1.0.0
data TopLevelCxt = TopLevelCxt
  { -- | The identifier for the next type declaration.
    nextTypeDecl :: Lvl
  , -- | The identifier for the next data constructor.
    nextConstructor :: Lvl
  , -- | The identifier for the next top-level definition.
    nextTopLevelDef :: Lvl
  }

-- | Namespaces, mapping names to their properties needed for elaboration.
--
-- @since 1.0.0
data Namespaces = Namespaces
  { -- | Type namespace.
    types :: HashMap TyName TyEntry
  , -- | Term namespace.
    terms :: HashMap Name TmEntry
  }

-- | A namespace entry for type identifiers, containing all
-- relevant information about the type.
--
-- @since 1.0.0
data TyEntry
  = -- | A data type, storing its top-level identifier, the number of
    -- type parameters it takes and the source position where it is defined.
    DataEntry Lvl Lvl BegPos
  | -- | A type variable, storing the size of the context when it was bound.
    TyVarEntry Lvl

-- | A namespace entry for term identifiers, containing all
-- relevant information about the term.
--
-- @since 1.0.0
data TmEntry
  = -- | A top-level definition, storing its top-level identifier, type
    -- and the source position where it is defined.
    TopLevelEntry Lvl Ty BegPos
  | -- | A data constructor, storing its top-level identifier, type
    -- and the source position where it is defined.
    ConstructorEntry Lvl Ty BegPos
  | -- | A variable, storing the size of the context when it was bound
    -- and its type.
    VarEntry Lvl Ty

tmEntryType :: TmEntry -> Ty
tmEntryType = \case
  TopLevelEntry _ a _     -> a
  ConstructorEntry _ a _  -> a
  VarEntry _ a            -> a

-- | The printing context, containing information needed for pretty-printing.
--
-- @since 1.0.0
data PrintCxt = PrintCxt
  { -- | Names of bound type variables.
    tyVars :: [TyName]
  , -- | Names of defined types.
    typeNames :: [TyName]
  , -- | Names of defined data constructors.
    constructorNames :: [Name]
  , -- | Names of bound term variables.
    tmVars :: [Name]
  , -- | Names of top-level definitions.
    topLevelDefNames :: [Name]
  }

-- | The elaboration context. Contains all global and local information
-- that is needed to elaborate a given declaration.
--
-- @since 1.0.0
data ElabCxt = ElabCxt
  { -- | The top-level context.
    topLevel :: TopLevelCxt
  , -- | The namespaces.
    names :: Namespaces
  , -- | The printing context.
    printInfo :: PrintCxt
  , -- | The current type evaluation environment.
    tyEnv :: TEnv
  , -- | The size of the current type context.
    tyLvl :: Lvl
  , -- | The size of the current term context.
    tmLvl :: Lvl
  }
