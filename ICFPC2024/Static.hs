module ICFPC2024.Static (
  StaticData,
  readStaticData,
) where

import ICFPC2024.Config ( Config(..) )
import qualified ICFPC2024.Puzzles.Static as P

import Data.Functor.Identity ( runIdentity )
import Data.Binary.Builder ( Builder )
import qualified Text.MMark as MMark
import qualified Data.Text.IO as T
import System.FilePath ( (</>) )

import Lucid.Base ( execHtmlT )

-- | The static data global data we keep in memory
type StaticData = (P.StaticData, (Builder, Builder))

-- | Read the global static data
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  let dir = staticFileDir cfg
  task <- renderMarkdownFile $ dir </> "task.md"
  language <- renderMarkdownFile $ dir </> "language.md"
  puzzle <- P.readStaticData cfg
  pure (puzzle, (task, language))

-- | Read a markdown file from disk and make html out of it
renderMarkdownFile :: FilePath -> IO Builder
renderMarkdownFile fn = do
  t <- T.readFile fn
  case MMark.parse "-" t of
    Left e -> error $ "renderMarkdownFile: " <> show e
    Right mmark -> do
      let html = MMark.render mmark
      pure $ runIdentity $ execHtmlT html
