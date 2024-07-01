module ICFPC2024.Puzzles.Hello.Static (
  readStaticData,
  StaticData (..),
) where

import ICFPC2024.AST ( Term )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, readTerm, Template )

import System.FilePath ( (</>) )

-- | The static data that we keep in memory for this subtask
data StaticData = StaticData
  { sdIndex :: Template
  , sdEcho  :: Template
  , sdEchoR :: Template
  , sdScoreboard :: Template
  , sdSelfCheck :: Term
  }

-- | Read the static data for this subtask from disk
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  tindex <- readTemplate cfg "hello/hello.md"
  techo  <- readTemplate cfg "hello/echo.md"
  score  <- readTemplate cfg "hello/scoreboard.md"
  techor <- readTemplate cfg "hello/echo_response.md"
  let baseDir = staticFileDir cfg
  selfc  <- readTerm $ baseDir </> "hello/selfcheck.icfp"

  pure StaticData
    { sdIndex = tindex
    , sdEcho  = techo
    , sdEchoR = techor
    , sdScoreboard = score
    , sdSelfCheck = selfc
    }
