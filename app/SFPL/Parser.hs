{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}

-- | The parser for the language.
module SFPL.Parser where

import Control.Applicative (liftA2, (<**>))
import Control.Monad.Combinators
import Data.Char
import Data.Function (on)
import Data.List (sortBy, groupBy, foldl')
import Data.Void
import SFPL.Base
import SFPL.Parser.Expr
import SFPL.Syntax.Raw
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | The parser type.
type Parser = Parsec Void String

type ParserE a = Parser (EndPos, a)
type ParserB a = Parser (BegPos -> a)
type ParserS a = ParserE (BegPos -> EndPos -> a)

----------------------------------------
-- Basic parsers and helpers

infixl 4 <$|>
infixl 4 <$|
infixl 4 <*|>

(<$|>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$|>) = (<$>) . (<$>)

(<$|) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$|) = (<$>) . (<$)

(<*|>) :: (Applicative f, Functor g) => f (a -> b) -> f (g a) -> f (g b)
(<*|>) = liftA2 (<$>)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--")
                    (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

signed :: Num a => Parser a -> Parser a
signed = L.signed sc

topLevel :: Parser a -> Parser a
topLevel p = sc *> p <* eof

sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = (:) <$> p <*> some (sep *> p)

whole :: Parser a -> Parser a
whole p = getOffset >>= \o -> region (setErrorOffset o) p

withE :: Parser a -> ParserE a
withE p = flip (,) <$> p <*> getSourcePos

withB :: ParserB a -> Parser a
withB p = getSourcePos <**> p

apE :: ParserS a -> ParserB a
apE p = (\(y, f) -> flip f y) <$> p

lexemeE :: Parser a -> ParserE a
lexemeE = lexeme . withE

sepBy1E :: Parser sc -> Parser a -> Parser sep -> ParserE [a]
sepBy1E sc p sep = (:) <$> try (sc *> p) <*|> go <* sc
  where
    go = do
      next <- eitherP (try $ sc *> sep *> p) getSourcePos
      case next of
        Left x  -> (x :) <$|> go
        Right p -> pure (p, [])

sepByE :: Parser sc -> Parser a -> Parser sep -> ParserE [a]
sepByE sc p sep = sepBy1E sc p sep
              <|> (, []) <$> getSourcePos <* sc

parens' :: Parser a -> Parser a
parens' = between (symbol "(") (char ')')

brackets' :: Parser a -> Parser a
brackets' = between (symbol "[") (char ']')

braces' :: Parser a -> Parser a
braces' = between (symbol "{") (char '}')

parens :: Parser a -> Parser a
parens = lexeme . parens'

braces :: Parser a -> Parser a
braces = lexeme . braces'

parensE :: Parser a -> ParserE a
parensE = lexemeE . parens'

bracketsE :: Parser a -> ParserE a
bracketsE = lexemeE . brackets'

bracesE :: Parser a -> ParserE a
bracesE = lexemeE . braces'

isIdenStart :: Char -> Bool
isIdenStart c = isLetter c || c == '_'

isIdenChar :: Char -> Bool
isIdenChar c = isAlphaNum c || c == '_'

isOpChar :: Char -> Bool
isOpChar = (`elem` ['+', '-', '*', '/', '^', '=', '!', '<', '>', '&', '|', ':', '~', '%'])

isSpecialChar :: Char -> Bool
isSpecialChar c = isOpChar c || c `elem` ['@', '(', ')', '[', ']', '{', '}', '\\', '.', ',', ';']

isValidToken :: Char -> Bool
isValidToken c = isIdenChar c || isSpecialChar c || isSpace c

checkToken :: Parser a
checkToken = do
  mc <- observing $ lookAhead anySingle
  case mc of
    Left _  -> empty
    Right c -> if isValidToken c
                 then try empty
                 else fail "invalid token"

keyword' :: Keyword -> Parser Keyword
keyword' s = try $ string s <* notFollowedBy (satisfy isIdenChar)

keyword :: Keyword -> Parser Keyword
keyword = lexeme . keyword'

keywordE :: Keyword -> ParserE Keyword
keywordE = lexemeE . keyword'

underscore' :: Parser Name
underscore' = whole $ keyword' "_"

underscore :: Parser Name
underscore = whole $ keyword "_"

operator :: String -> Parser String
operator s = lexeme . try $ string s <* notFollowedBy (satisfy isOpChar)

tryKeywords :: (a -> Keyword) -> [a] -> Parser a
tryKeywords f = choice . map g
  where
    g x = x <$ keyword (f x)

tryKeywordsE' :: [Keyword] -> [a] -> ParserE a
tryKeywordsE' xs ys = choice $ zipWith f xs ys
  where
    f x y = y <$| keywordE x

tryKeywordsE :: (a -> Keyword) -> [a] -> ParserE a
tryKeywordsE f ys = tryKeywordsE' (map f ys) ys

testP :: Show a => Parser a -> String -> IO ()
testP p s = putStrLn $ either errorBundlePretty show $ parse p "" s

testPI :: Show a => Parser a -> IO ()
testPI p = getSource >>= testP p
  where
    getSource = init . unlines <$> go 
    go = do
      line <- getLine
      case line of
        ";" -> pure []
        _   -> (line :) <$> go

------------------------------------------------------------
-- Parsing types

tyIden :: Parser TyName
tyIden = iden <?> "type identifier"

tyBinder :: Parser TyName
tyBinder = (tyIden <?> "type variable binding") <|> underscore

tyAtom :: ParserB Ty
tyAtom = THole <$ underscore
     <|> TyIden <$> tyIden
     <|> apE (tryKeywordsE' [kwInt, kwFloat, kwChar] [Int, Float, Char]
          <|> Tuple <$|> parensE (topTy `sepBy` symbol ",")
          <|> List <$|> bracketsE topTy)
     <|> checkToken 

tySpine :: ParserB Ty
tySpine = app <$> tyAtom <*> many (withB tyAtom)
  where
    app a = \case
      []  -> a
      as  -> TApp <$> a <*> pure as

world :: ParserB Ty
world = World <$> (symbol "%" *> withB tySpine)
    <|> tySpine

fun :: ParserB Ty
fun = do
  a <- world
  option a $ (\b -> Fun <$> a <*> pure b) <$> (symbol "->" *> withB forAll)

forAll :: ParserB Ty
forAll = ForAll <$> (symbol "@" *> some tyBinder) <*> (symbol "." *> ty)
     <|> fun

ty :: Parser Ty
ty = withB forAll

topTy :: Parser Ty
topTy = ty <|> fail "expecting type"

------------------------------------------------------------
-- Parsing patterns

ctrArgs :: ParserE CtrArgs
ctrArgs = foldr step [] <$|> sepByE sc arg (pure ())
  where
    arg = eitherP binder' (braces' $ some tyBinder)
    step = either ((:) . Left) ((++) . map Right)

ctrPat :: ParserS Pattern
ctrPat = CtrPat <$> iden' <*|> ctrArgs

pat :: Parser Pattern
pat = withB (apE (ConsPat <$> try (binder <* symbol "::") <*|> binderE)
         <|> WildcardPat <$ underscore
         <|> apE (FloatPat <$|> floatLit
              <|> IntPat <$|> intLit
              <|> CharPat <$|> charLit
              <|> TuplePat <$|> parensE (binder `sepBy2` symbol "," <|> pure [])
              <|> EmptyListPat <$| bracketsE (pure ())
              <|> ctrPat))
  <|> checkToken

------------------------------------------------------------
-- Parsing terms

iden' :: Parser Name
iden' = try . label "identifier" . whole $ do
  c <- satisfy isIdenStart
  cs <- takeWhileP Nothing isIdenChar
  let x = c : cs
  if x `elem` keywords
    then fail "invalid identifier"
    else pure x

iden :: Parser Name
iden = lexeme iden'

idenE :: ParserE Name
idenE = lexemeE iden'

binder' :: Parser Name
binder' = (iden' <?> "variable binding") <|> underscore'

binder :: Parser Name
binder = lexeme binder'

binderE :: ParserE Name
binderE = lexemeE binder'

intLit :: ParserE Integer
intLit = lexemeE . try $ hidden L.decimal

floatLit :: ParserE Double
floatLit = lexemeE . try $ L.float

character :: Parser Char
character = lookAhead (anySingleBut '\n') *> L.charLiteral

charLit :: ParserE Char
charLit = lexemeE $ char '\'' *> character <* char '\''

stringLit :: ParserE String
stringLit = lexemeE $ char '"' *> manyTill character (char '"')

nullFunc :: ParserE NullaryFunc
nullFunc = tryKeywordsE nullFuncName [minBound .. maxBound]

atom :: ParserB Tm
atom = Hole <$ underscore
   <|> Iden <$> iden
   <|> apE (NullFunc <$|> nullFunc
        <|> FloatLit <$|> floatLit
        <|> IntLit <$|> intLit
        <|> CharLit <$|> charLit
        <|> StringLit <$|> stringLit
        <|> Tup <$|> parensE (topTm `sepBy` symbol ",")
        <|> ListLit <$|> bracketsE (topTm `sepBy` symbol ","))
   <|> checkToken

unFunc :: Parser UnaryFunc
unFunc = tryKeywords unFuncName [minBound .. maxBound]

binFunc :: Parser BinaryFunc
binFunc = tryKeywords binFuncName [minBound .. maxBound]

switchBranch :: Parser SwitchBranch
switchBranch = (,) <$> tm <*> (symbol "->" *> tm)

caseBranch :: Parser CaseBranch
caseBranch = (,) <$> pat <*> (symbol "." *> tm)

branches :: Parser a -> ParserE [a]
branches p = bracesE $ p `sepEndBy` symbol ";"

switch :: ParserS Tm
switch = Switch <$|> (keyword "switch" *> branches switchBranch)

kase :: ParserS Tm
kase = Case <$> (keyword "case" *> tm) <*|> (keyword "of" *> branches caseBranch)

param :: Parser (Either Tm (EndPos, [Ty]))
param = eitherP (withB atom) (bracesE $ topTy `sepBy1` symbol ",")

app :: ParserB Tm
app = UnFunc <$> unFunc <*> withB atom
  <|> BinFunc <$> binFunc <*> withB atom <*> withB atom
  <|> apE (switch <|> kase)
  <|> foldl' step <$> atom <*> many param
  where
    step t = \case
      Left u        -> App <$> t <*> pure u
      Right (p, as) -> AppI <$> t <*> pure as <*> pure p

table :: [[Operator Parser Tm]]
table = map (map toOperator) opTable
  where
    unOpPrec (unOpDetails -> (p, _)) = p
    binOpPrec (binOpDetails -> (p, _, _)) = p
    prec = either unOpPrec binOpPrec
    unOps = [minBound .. maxBound]
    binOps = [minBound .. maxBound]
    ops = map Left unOps ++ map Right binOps
    opTable = groupBy ((==) `on` prec) $ sortBy (flip compare `on` prec) ops
    toOperator = \case
      Left op   -> let (_, opSymbol) = unOpDetails op
                   in Prefix $ UnOp op <$ operator opSymbol
      Right op  -> let (_, assoc, opSymbol) = binOpDetails op
                   in Infix assoc $ BinOp op <$ operator opSymbol

expr :: ParserB Tm
expr = makeExprParser app table

tyAnn :: ParserB Tm
tyAnn = do
  t <- expr
  option t $ (\a -> TyAnn <$> t <*> pure a) <$> tySig

tySig :: Parser Ty
tySig = symbol ":" *> ty

optTySig :: Parser (Maybe Ty)
optTySig = optional tySig

lamBindExpl :: Parser LamBind
lamBindExpl = Expl <$> binder <*> pure Nothing
          <|> parens (Expl <$> binder <*> (Just <$> tySig))

lamBindImpl :: Parser [LamBind]
lamBindImpl = braces $ some (Impl <$> tyBinder)

lamBinds :: Parser [LamBind]
lamBinds = foldr (either (:) (++)) [] <$> some (eitherP lamBindExpl lamBindImpl)

lam :: ParserB Tm
lam = Lam <$> (symbol "\\" *> lamBinds) <*> (symbol "." *> tm)

iff :: ParserB Tm
iff = If <$> (keyword "if" *> tm)
         <*> (keyword "then" *> tm)
         <*> (keyword "else" *> tm)

split :: ParserB Tm
split = Split <$> (keyword "split" *> tm)
              <*> (keyword "as" *> parens (binder `sepBy2` symbol ","))
              <*> (symbol "." *> tm)

letBind :: Parser LocalBind
letBind = (,,) <$> binder <*> optTySig <*> (symbol "=" *> tm)

doBind :: Parser LocalBind
doBind = (,,) <$> binder <*> optTySig <*> (symbol "<-" *> tm)

lett :: ParserB Tm
lett = Let <$> (keyword "let" *> letBind `sepEndBy1` symbol ";")
           <*> (keyword "in" *> tm)

doo :: ParserB Tm
doo = Do <$> (keyword "do" *> doBind `sepEndBy1` symbol ";")
         <*> (keyword "then" *> tm)

tm :: Parser Tm
tm = withB $ lam <|> iff <|> split <|> lett <|> doo
         <|> tyAnn

topTm :: Parser Tm
topTm = tm <|> fail "expecting term"

topLevelDef :: Parser TopLevelDef
topLevelDef = withB . apE . lexemeE $
  TL <$> iden <*> tySig <*> (symbol "=" *> tm) <* char ';'

----------------------------------------
-- Type declarations

constructor :: Parser Constructor
constructor = withB $ Constructor <$> iden <*> tySig

constructors :: Parser [Constructor]
constructors = (symbol "=" *> constructor `sepBy1` symbol "|") <|> pure []

dataDecl :: Parser DataDecl
dataDecl = withB . apE . lexemeE $
  DD <$> (keyword "data" *> tyIden) <*> many tyBinder <*> constructors <* char ';'

typeDecl :: Parser TypeDecl
typeDecl = DataDecl <$> dataDecl

----------------------------------------
-- Programs

program :: Parser Program
program = many $ eitherP typeDecl topLevelDef
