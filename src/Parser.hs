{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Parser where

import           Control.Monad                  ( mzero )
import           Data.Either                    ( either )
import           Data.Functor                   ( ($>) )
import           Data.Functor.Identity          ( Identity )
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Text.Parsec
import           Text.Parsec.Language          as Lang
import           Text.Parsec.Text
import           Text.Parsec.Token             as Tok
import           Types

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd   = "-}"
    , Tok.commentLine  = ";"
    , Tok.opStart      = mzero
    , Tok.opLetter     = mzero
    , Tok.identStart   = letter <|> oneOf "!$%&*/:<=>?^_~"
    , Tok.identLetter  = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
    }

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

parens = Tok.parens lexer
ws = Tok.whiteSpace lexer
lexe = Tok.lexeme lexer
ident =
    T.pack <$> (Tok.identifier lexer <|> lexe (try $ string "+" <|> string "-"))

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

number = Tok.decimal lexer
numeric = do
    sign <- optionMaybe (char '-')
    case sign of
        (Just v) ->
            either (Int . (*) (-1) . fromInteger)
                   (Float . (*) (-1) . realToFrac)
                <$> Tok.naturalOrFloat lexer
        Nothing -> either (Int . fromInteger) (Float . realToFrac)
            <$> Tok.naturalOrFloat lexer
boolean = lexe $ char '#' *> ((char 't' $> True) <|> (char 'f' $> False))

nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

lisp :: Parser LispVal
lisp =
    Nil <$  nil
  <|> Bool <$> boolean
  <|> try numeric
  <|> String . T.pack <$> stringLiteral lexer 
  <|> Atom <$> ident
  <|> quoteF <$> quoted lisp
  <|> List <$> Parser.parens (lisp `sepBy` ws)

quoteF x = List [Atom "quote", x]

replParser :: T.Text -> Either ParseError LispVal
replParser = parse (ws *> lexe lisp) "input"
