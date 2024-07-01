module ICFPC2024.Puzzles.Lambdaman.Static (
  readStaticData,
  StaticData (..),
  Problem (..),
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Interpreter ( interpret )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, Template, readTerm )

import GHC.Generics ( Generic )

import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Data.Yaml ( decodeFileThrow )
import Data.Map ( Map )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map

import Control.Monad ( forM )
import Control.Applicative ( (<|>) )

import System.FilePath ( (</>) )

-- | Format of the YAML config for this subtask
data ProblemConfig
  = ProblemTxt { txt :: FilePath }
  | ProblemExpression { expression :: FilePath }
  deriving ( Generic )

instance FromJSON ProblemConfig where
  parseJSON v =
        withObject "ProblemTxt"        (\o -> ProblemTxt <$> o .: "txt") v
    <|> withObject "ProblemExpression" (\o -> ProblemExpression <$> o .: "expression") v

-- | Internal data we store for a problem
data Problem = Problem
  { pExpression :: Term
  , pGrid :: Map (Int,Int) Char
  , pBegin :: (Int, Int)
  } deriving ( Generic )

-- | The static data that we keep in memory for this subtask
data StaticData = StaticData Template (Map ByteString Problem)

-- | Read the static data for this subtask from disk
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  -- Read the description
  desc <- readTemplate cfg "lambdaman/lambdaman.md"

  -- And read all the problems
  let baseDir = staticFileDir cfg </> "lambdaman"
  pconf <- decodeFileThrow (baseDir </> "config.yaml")
  ps <- forM (Map.toList pconf) $ \(nm, pc) -> do
    -- Read the problem from file
    (term, str) <- case pc of
      ProblemTxt {}        -> do
        str <- BS8.readFile (baseDir </> txt pc)
        pure (TString str, str)
      ProblemExpression {} -> do
        term <- readTerm (baseDir </> expression pc)
        let str = case interpret False term of
              Right (TString s, _) -> s
              _ -> error $ "readStaticData: interpreting failed"
        pure (term, str)
    let grid = Map.fromList
          [ ((r,c),ch)
          | (r,row) <- zip [0..] (lines $ BS8.unpack str)
          , (c,ch) <- zip [0..] row
          ]
    let begin = head [ p | (p,'L') <- Map.toList grid ]
    pure (BS8.pack nm, Problem
      { pExpression = term
      , pGrid       = grid
      , pBegin      = begin
      })
  pure $ StaticData desc $ Map.fromList ps
