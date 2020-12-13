{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Parse
  ( parseTerms
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

-- Building blocks

pName :: Parser T.Text
pName = lexeme $ unquotedName <|> quotedName
  where
    unquotedName = takeWhile1P (Just "lowercase character") isLower
    quotedName = fmap T.pack $ C.char '\'' *> manyTill L.charLiteral (C.char '\'')

pVarName :: Parser T.Text
pVarName = lexeme $ takeWhile1P (Just "uppercase character") isUpper

pCons :: Parser (Term T.Text)
pCons = brackets $ do
  elems <- pTerm `sepBy1` symbol ","
  void $ symbol "|"
  rest <- pTerm
  pure $ foldr (\a b -> Stat "[|]" [a, b]) rest elems

pList :: Parser (Term T.Text)
pList = do
  elems <- brackets $ pTerm `sepBy` symbol ","
  pure $ foldr (\a b -> Stat "[|]" [a, b]) (Stat "[]" []) elems

pStat :: Parser (T.Text, [Term T.Text])
pStat = do
  name <- pName
  terms <- parens (pTerm `sepBy1` symbol ",") <|> pure []
  pure (name, terms)

pTerm :: Parser (Term T.Text)
pTerm
  =   (Var <$> pVarName)
  <|> (uncurry Stat <$> pStat)
  <|> try pCons
  <|> pList
  <|> parens pExpr

pExpr :: Parser (Term T.Text)
pExpr = makeExprParser pTerm
  [ [ binary "=" ]
  ]
  where
    binary name = InfixL $ (\a b -> Stat name [a, b]) <$ symbol name

pTerms :: Parser [Term T.Text]
pTerms = (pExpr `sepBy1` symbol ",") <* symbol "."

pDef :: Parser (Def T.Text)
pDef = do
  name <- pName
  args <- parens (pExpr `sepBy1` symbol ",") <|> pure []
  terms <- (symbol ":-" *> (pExpr `sepBy1` symbol ",")) <|> pure []
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
