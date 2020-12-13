{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Debug
  ( parseAndRun
  ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           Propa.Prolog.Display
import           Propa.Prolog.Parse
import           Propa.Prolog.Unify

parseAndRun :: T.Text -> T.Text -> IO ()
parseAndRun dbText termsText = T.putStrLn $ either id id $ do
  db <- parseDb "<input>" dbText
  terms <- parseTerms "<input>" termsText
  pure $ T.intercalate "\n" $ map displayResult $ run db terms