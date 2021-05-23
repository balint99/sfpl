
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
  )
  where

import SFPL.Parser.Internal
