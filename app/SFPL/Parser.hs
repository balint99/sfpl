
-- | The parser for the language.
module SFPL.Parser
  ( -- * Parsers
    ty,
    pat,
    tm,
    constructor,
    dataDecl,
    typeDecl,
    program,
    
    -- * Helpers
    parseProgram
  )
  where

import Data.Void
import SFPL.Parser.Internal
import SFPL.Syntax.Raw
import Text.Megaparsec

-- | Parse a complete program given the name
-- and contents of the source file.
--
-- @since 1.0.0
parseProgram ::
  String -> String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse (topLevel program)
