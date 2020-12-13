{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Parse
  ( parseTerms
  , parseDb
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Void

import qualified Data.Text                  as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Propa.Prolog.Types

type Parser = Parsec Void T.Text

-- Lexeme stuff

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "%") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Building blocks

pName :: Parser T.Text
pName = lexeme $ unquotedName <|> quotedName
  where
    unquotedName = takeWhile1P (Just "lowercase character") isLower
    quotedName = fmap T.pack $ C.char '"' *> manyTill L.charLiteral (C.char '"')

pVarName :: Parser T.Text
pVarName = lexeme $ takeWhile1P (Just "uppercase character") isUpper

pStat :: Parser (T.Text, [Term T.Text])
pStat = do
  name <- pName
  terms <- parens (pTerm `sepBy1` symbol ",") <|> pure []
  pure (name, terms)

pTerm :: Parser (Term T.Text)
pTerm = (Var <$> pVarName) <|> (uncurry Stat <$> pStat)

pTerms :: Parser [Term T.Text]
pTerms = (pTerm `sepBy1` symbol ",") <* symbol "."

pDef :: Parser (Def T.Text)
pDef = do
  name <- pName
  args <- parens (pTerm `sepBy1` symbol ",") <|> pure []
  terms <- (symbol ":-" *> (pTerm `sepBy1` symbol ",")) <|> pure []
  void $ symbol "."
  pure $ Def name args terms

pDefs :: Parser [Def T.Text]
pDefs = many pDef

-- And finally, our nice parsers

parseHelper :: Parser a -> FilePath -> T.Text -> Either T.Text a
parseHelper p path input
  = first (T.pack . errorBundlePretty)
  $ parse (space *> p <* eof) path input

parseTerms :: FilePath -> T.Text -> Either T.Text [Term T.Text]
parseTerms = parseHelper pTerms

parseDb :: FilePath -> T.Text -> Either T.Text (Db T.Text)
parseDb = parseHelper pDefs
