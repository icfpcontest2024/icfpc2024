module ICFPC2024.Puzzles.Spaceship.Static (
  readStaticData,
  StaticData (..),
  Problem (..),
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Interpreter ( interpret )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, Template, readTerm )

import GHC.Generics ( Generic )

import Data.Yaml ( decodeFileThrow )
import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Data.Map ( Map )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map

import Control.Applicative ( (<|>) )
import Control.Monad ( forM )

import System.FilePath ( (</>) )

-- | Format of the YAML config for this subtask
data ProblemConfig
  = ProblemFile { file :: FilePath }
  | ProblemExpression { expr :: FilePath }
  deriving ( Generic )

instance FromJSON ProblemConfig where
  parseJSON v =
        withObject "ProblemFile"       (\o -> ProblemFile <$> o .: "file") v
    <|> withObject "ProblemExpression" (\o -> ProblemExpression <$> o .: "expression") v

-- | Internal data we store for a problem
data Problem = Problem
  { expression :: Term
  , targets :: [(Integer,Integer)]
  } deriving ( Generic )

-- | The static data that we keep in memory for this subtask
data StaticData = StaticData Template (Map ByteString Problem)

-- | Read the static data for this subtask from disk
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  -- Read the description
  desc <- readTemplate cfg "spaceship/spaceship.md"

  -- And read all the problems
  let baseDir = staticFileDir cfg </> "spaceship"
  pconf <- decodeFileThrow (baseDir </> "config.yaml")
  ps <- forM (Map.toList pconf) $ \(nm, pc) -> do
    (term,pts) <- case pc of
      ProblemFile {} -> do
        pts <- BS8.readFile (baseDir </> file pc)
        pure (TString pts, pts)
      ProblemExpression {} -> do
        term <- readTerm (baseDir </> expr pc)
        let str = case interpret False term of
              Right (TString s, _) -> s
              _ -> error $ "readStaticData: interpreting failed"
        pure (term, str)
    pure (BS8.pack nm, Problem term (readPoints pts))
  pure $ StaticData desc $ Map.fromList ps

-- | Read a testcase from file
readPoints :: ByteString -> [(Integer,Integer)]
readPoints = map readPos . BS8.lines where
  readPos = readPair . map read . words . BS8.unpack
  readPair [x,y] = (x,y)
  readPair _ = error "readPoints failed"
