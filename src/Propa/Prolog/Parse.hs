{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Parse
  ( parseStats
  , parseDb
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Void

import           Control.Monad.Combinators.Expr
import qualified Data.Text                      as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- Names

pName :: Parser T.Text
pName = lexeme $ unquotedName <|> quotedName
  where
    unquotedName = takeWhile1P (Just "lowercase character") isLower
    quotedName = fmap T.pack $ C.char '\'' *> manyTill L.charLiteral (C.char '\'')

pVarName :: Parser T.Text
pVarName = lexeme $ takeWhile1P (Just "uppercase character") isUpper

-- Statements

pTermToStat :: Parser (Term T.Text) -> Parser (Stat T.Text)
pTermToStat p = do
  term <- p
  case term of
    (TVar _)  -> fail "expected statement, not variable"
    (TInt _)  -> fail "expected statement, not integer"
    (TStat s) -> pure s

-- | Parse a statement of the form @name(args)@.
pPlainStat :: Parser (Stat T.Text)
pPlainStat = do
  name <- pName
  terms <- parens (pTerm `sepBy1` symbol ",") <|> pure []
  pure $ Stat name terms

pStat :: Parser (Stat T.Text)
pStat = pPlainStat <|> pTermToStat pTerm

pStats :: Parser [Stat T.Text]
pStats = (pStat `sepBy1` symbol ",") <* symbol "."

-- Terms

pCons :: Parser (Term T.Text)
pCons = brackets $ do
  elems <- pTerm `sepBy1` symbol ","
  void $ symbol "|"
  rest <- pTerm
  pure $ foldr (\a b -> TStat $ Stat "[|]" [a, b]) rest elems

pList :: Parser (Term T.Text)
pList = do
  elems <- brackets $ pTerm `sepBy` symbol ","
  pure $ foldr (\a b -> TStat $ Stat "[|]" [a, b]) (TStat $ Stat "[]" []) elems

-- | Parse a term that is not an expression.
pPlainTerm :: Parser (Term T.Text)
pPlainTerm
  =   (TVar <$> pVarName)
  <|> (TInt <$> L.signed (pure ()) L.decimal)
  <|> (TStat <$> pPlainStat)
  <|> try pCons
  <|> pList
  <|> parens pTerm

pTerm :: Parser (Term T.Text)
pTerm = makeExprParser pPlainTerm
  [ [ binary "=" ]
  ]
  where
    binary name = InfixL $ (\a b -> TStat $ Stat name [a, b]) <$ symbol name

-- Definitions

pDef :: Parser (Def T.Text)
pDef = do
  stat <- pStat
  stats <- (symbol ":-" *> (pStat `sepBy1` symbol ",")) <|> pure []
  void $ symbol "."
  pure $ Def stat stats

pDefs :: Parser [Def T.Text]
pDefs = many pDef

-- And finally, our nice parsers

parseHelper :: Parser a -> FilePath -> T.Text -> Either T.Text a
parseHelper p path input
  = first (T.pack . errorBundlePretty)
  $ parse (space *> p <* eof) path input

parseStats :: FilePath -> T.Text -> Either T.Text [Stat T.Text]
parseStats = parseHelper pStats

parseDb :: FilePath -> T.Text -> Either T.Text (Db T.Text)
parseDb = parseHelper pDefs
