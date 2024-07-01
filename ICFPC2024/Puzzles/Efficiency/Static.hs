module ICFPC2024.Puzzles.Efficiency.Static (
  readStaticData,
  StaticData (..),
  Problem (..),
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, Template, readTerm )

import GHC.Generics ( Generic )

import Data.Yaml ( decodeFileThrow, FromJSON )
import Data.Map ( Map )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map

import Control.Monad ( forM )

import System.FilePath ( (</>) )

-- | Format of the YAML config for this subtask
data ProblemConfig = ProblemConfig
  { expression :: FilePath
  , answer :: Integer
  } deriving ( Generic )

instance FromJSON ProblemConfig

-- | Internal data we store for a problem
data Problem = Problem
  { pExpression :: Term
  , pAnswer :: Integer
  } deriving ( Generic )

-- | The static data that we keep in memory for this subtask
data StaticData = StaticData Template Template Template (Map ByteString Problem)

-- | Read the static data for this subtask from disk
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  -- Read the description
  desc     <- readTemplate cfg "efficiency/efficiency.md"
  pwd      <- readTemplate cfg "efficiency/password.md"
  backdoor <- readTemplate cfg "efficiency/backdoor.md"

  -- And read all the problems
  let baseDir = staticFileDir cfg </> "efficiency"
  pconf <- decodeFileThrow (baseDir </> "config.yaml")
  ps <- forM (Map.toList pconf) $ \(nm, pc) -> do
    term <- readTerm (baseDir </> expression pc)
    pure (BS8.pack nm, Problem term (answer pc))
  pure $ StaticData desc pwd backdoor $ Map.fromList ps
