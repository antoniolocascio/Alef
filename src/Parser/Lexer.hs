module Parser.Lexer where

import qualified Data.Text                     as T

import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Text              as PT
import           AST

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser Tok.LanguageDef
  { Tok.commentStart    = "(*"
  , Tok.commentEnd      = "*)"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ":,;="
  , Tok.opLetter        = char '='
  , Tok.reservedNames   = [ "val"
                          , "let"
                          , "in"
                          , "fun"
                          , "succ"
                          , "handler"
                          , "with"
                          , "if"
                          , "then"
                          , "else"
                          , "handle"
                          , "match"
                          , "()"
                          , "unit"
                          , "nat"
                          , "bool"
                          , "empty"
                          , "true"
                          , "false"
                          , "signature"
                          ]
  , Tok.reservedOpNames = ["->", "->>", "=", "|"]
  , Tok.caseSensitive   = True
  }

reservedOp = Tok.reservedOp lexer
reserved = Tok.reserved lexer

toNat :: Integer -> Exp
toNat 0 = EZero
toNat n = ESucc (toNat $ n - 1)


number = do
  n <- Tok.natural lexer
  if n < 0 then fail "Expected a natural number (>= 0)." else return (toNat n)
parens = Tok.parens lexer
commaSep = Tok.commaSep lexer
commaSep1 = Tok.commaSep1 lexer
semiSep = Tok.semiSep lexer
semiSep1 = Tok.semiSep1 lexer
identifier = Tok.identifier lexer
dot = Tok.dot lexer
colon = Tok.colon lexer
comma = Tok.comma lexer
brackets = Tok.brackets lexer
braces = Tok.braces lexer
pbrackets = between (symbol "<") (symbol ">")
symbol = Tok.symbol lexer
stringLiteral = Tok.stringLiteral lexer
whiteSpace = Tok.whiteSpace lexer


