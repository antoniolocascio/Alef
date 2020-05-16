module Parser.Parser where

import           Prelude                 hiding ( exp )
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

import qualified Text.Parsec.Expr              as Ex
import qualified Text.Parsec.Token             as Tok

import           Parser.Lexer
import           AST
import           Types
import           SugarTypes
import           Utils.Symbol
import           Operation
import           EffectRow
import           Utils.Error

-- Type Parser

operation :: Parser Operation
operation = try $ do
  initial <- upper
  rest    <- many alphaNum
  whiteSpace
  return $ toSymbol (initial : rest)

row :: Parser EffRow
row = pbrackets $ try neRow <|> eRow
 where
  neRow = do
    ops <- commaSep operation
    reservedOp "|"
    makeRow ops . toSymbol <$> identifier
  eRow = emptyRow . toSymbol <$> identifier

boolT :: Parser VType
boolT = reserved "bool" >> return TBool

natT :: Parser VType
natT = reserved "nat" >> return TNat

unitT :: Parser VType
unitT = reserved "unit" >> return TUnit

emptyT :: Parser VType
emptyT = reserved "empty" >> return TEmpty

baseT :: Parser VType
baseT = boolT <|> natT <|> unitT <|> emptyT

funcT :: Parser VType
funcT = do
  a <- baseT <|> parens valueT
  reservedOp "->"
  TFunc a <$> compT

handT :: Parser VType
handT = do
  c <- compT
  reservedOp "->>"
  THand c <$> compT

valueT :: Parser VType
valueT = try funcT <|> try handT <|> parens valueT <|> baseT

compT :: Parser CType
compT = do
  a <- baseT <|> parens valueT
  TComp a <$> row

typ :: Parser Type
typ =
  try (VT <$> handT)
    <|> try (CT <$> compT)
    <|> try (VT <$> funcT)
    <|> (VT <$> parens valueT)
    <|> (VT <$> baseT)

optyp :: Parser OpType
optyp = do
  a <- baseT
  reservedOp "->"
  TOp a <$> baseT

typeParser :: Parser Type
typeParser = whiteSpace >> typ

parseType :: String -> Either ParseError Type
parseType = parse typeParser ""

-- Term Parser

--- Exp Parser
varE :: Parser Exp
varE = EVar . toSymbol <$> identifier

boolE :: Parser Exp
boolE =
  (reserved "true" >> return ETrue) <|> (reserved "false" >> return EFalse)

natE :: Parser Exp
natE = number <|> (reserved "succ" >> ESucc <$> exp)

unitE :: Parser Exp
unitE = symbol "()" >> return EUnit

funE :: Parser Exp
funE = do
  reserved "fun"
  x <- toSymbol <$> identifier
  reservedOp "->"
  EFunc x <$> comp

annoE :: Parser Exp
annoE = parens $ do
  e <- exp
  colon
  EAnno e <$> valueT

handlE :: Parser Exp
handlE = do
  reserved "handler"
  reserved "val"
  x <- toSymbol <$> identifier
  reservedOp "->"
  cv <- comp
  comma
  EHand x cv <$> clausesH

clausesH :: Parser [(Operation, Var, Var, Comp)]
clausesH = braces $ commaSep clauseH
 where
  clauseH = do
    op <- operation
    x  <- toSymbol <$> identifier
    k  <- toSymbol <$> identifier
    reservedOp "->"
    ci <- comp
    return (op, x, k, ci)

exp :: Parser Exp
exp =
  try boolE
    <|> try natE
    <|> try unitE
    <|> try funE
    <|> try annoE
    <|> try handlE
    <|> varE
    <|> parens exp


--- Comp Parser
valC :: Parser Comp
valC = do
  reserved "val"
  CVal <$> exp

opC :: Parser Comp
opC = try explOp <|> genericOp
 where
  explOp = do
    op     <- operation
    e      <- exp
    (y, c) <- parens $ do
      v <- toSymbol <$> identifier
      dot
      k <- comp
      return (v, k)
    return $ COp op e y c
  genericOp = do
    op <- operation
    e  <- exp
    let z = toSymbol "z"
    return $ COp op e z (CVal (EVar z))

withC :: Parser Comp
withC = do
  reserved "with"
  e <- exp
  reserved "handle"
  CWith e <$> comp

appC :: Parser Comp
appC = do
  e1 <- exp
  CApp e1 <$> exp

ifC :: Parser Comp
ifC = do
  reserved "if"
  e <- exp
  reserved "then"
  c1 <- comp
  reserved "else"
  CIf e c1 <$> comp

matchC :: Parser Comp
matchC = do
  reserved "match"
  e <- exp
  reserved "with"
  reservedOp "|"
  symbol "0"
  reservedOp "->"
  c1 <- comp
  reservedOp "|"
  reserved "succ"
  x <- toSymbol <$> identifier
  reservedOp "->"
  CMatch e c1 x <$> comp

letC :: Parser Comp
letC = do
  reserved "let"
  x <- toSymbol <$> identifier
  reservedOp "="
  c1 <- comp
  reserved "in"
  CLet x c1 <$> comp

comp :: Parser Comp
comp =
  valC
    <|> opC
    <|> withC
    <|> ifC
    <|> try matchC
    <|> letC
    <|> try appC
    <|> parens comp

term :: Parser Term
term = C <$> try comp <|> E <$> exp

termParser :: Parser Term
termParser = whiteSpace >> term

parseTerm :: String -> Either ParseError Term
parseTerm = parse termParser ""

-- Parse signature and declarations (more info in Sugar.hs)

sig :: Parser Sig
sig = do
  reserved "signature"
  braces $ commaSep ops
 where
  ops = do
    op <- identifier
    colon
    t <- optyp
    return (toSymbol op, t)

dec :: Parser Dec
dec = do
  x <- identifier
  colon
  tau <- typ
  symbol x
  reservedOp "="
  t <- term
  return (toSymbol x, tau, t)

decs :: Parser [Dec]
decs = sepBy dec (symbol ";;")

noSig :: Parser (Maybe Sig, [Dec])
noSig = decs >>= \d -> return (Nothing, d)

hasSig :: Parser (Maybe Sig, [Dec])
hasSig = do
  s  <- sig
  ds <- decs
  return (Just s, ds)

decsParser :: Parser (Maybe Sig, [Dec])
decsParser = whiteSpace >> (hasSig <|> noSig)

parseDecs :: String -> Either Error (Maybe Sig, [Dec])
parseDecs s = case parse decsParser "" s of
  Left  e    -> Left (show e)
  Right decs -> Right decs

