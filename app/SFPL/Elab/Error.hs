
module SFPL.Elab.Error
  ( -- * Types
    ElabError (..),
    ElabErrorType (..),
    SyntacticCategory (..),
    scName,
    MalformedTypeErrorReason (..),
    TypeHolePlace (..),
    UnificationErrorReason (..),
    OverloadType (..),
    ElabErrorItem (..),
    
    -- * Printing
    SourceFile,
    ErrorPCxt,
    prettyElabError,
  )
  where

import SFPL.Elab.Error.Types
import SFPL.Elab.Error.Pretty
import SFPL.Elab.Error.Instances
