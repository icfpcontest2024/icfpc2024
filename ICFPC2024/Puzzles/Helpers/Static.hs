module ICFPC2024.Puzzles.Helpers.Static (
  readTemplate,
  renderTemplate,
  Template,
  readTerm,
) where

import ICFPC2024.Config ( Config (..) )
import ICFPC2024.AST ( Term(..) )
import ICFPC2024.Parser ( pTerm )

import System.FilePath ( (</>), takeDirectory )
import System.Directory ( makeAbsolute )

import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Lazy.Char8 as BS8

import Text.Mustache

-- | Read a template file from the static dir
readTemplate :: Config -> FilePath -> IO Template
readTemplate cfg fp = do
  baseDir <- makeAbsolute $ staticFileDir cfg
  let fullPath = baseDir </> fp
  automaticCompile [baseDir, takeDirectory fullPath] fullPath >>= \case
    Left e -> error $ "readTemplate: automaticCompile failed: " <> show e
    Right t -> pure t

-- | Render a template to an ICFP term
renderTemplate :: ToMustache k => Template -> k -> Term
renderTemplate t vs = case checkedSubstitute t vs of
  ([], r) -> TString $ BS8.fromStrict $ encodeUtf8 r
  (es, _) -> error $ "renderTemplate: substitution error: " <> show es

-- | Read a static term from file
readTerm :: FilePath -> IO Term
readTerm fp = do
  mbTerm <- pTerm <$> BS8.readFile fp
  case mbTerm of
    Left e     -> error $ "readTerm failed for " <> fp <> ": " <> show e
    Right term -> pure term
