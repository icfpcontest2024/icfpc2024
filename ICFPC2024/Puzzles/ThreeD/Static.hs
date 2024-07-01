module ICFPC2024.Puzzles.ThreeD.Static (
  readStaticData,
  StaticData (..),
  Testcase (..),
  Problem (..),
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, Template, renderTemplate )

import GHC.Generics ( Generic )

import Data.Aeson ( FromJSON )
import Data.Yaml ( decodeFileThrow )
import Data.Map ( Map )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map

import Text.Mustache ( ToMustache(..), object, (~>) )

import Control.Monad ( forM )

import System.FilePath ( (</>) )

-- | Format of the YAML config for this subtask
data ProblemConfig = ProblemConfig
  { description :: FilePath
  , samples :: String
  , secret :: String
  } deriving ( Generic )

instance FromJSON ProblemConfig

-- | A single testcase
data Testcase = Testcase
  { tA :: Integer
  , tB :: Maybe Integer
  , tAns :: Integer
  } deriving ( Generic, Show )

instance ToMustache Testcase where
  toMustache p = object
    [ "a"   ~> tA p
    , "b"   ~> tB p
    , "ans" ~> tAns p
    ]

-- | Internal data we store for a problem
data Problem = Problem
  { pDescription :: Term
  , pSamples :: [Testcase]
  , pSecret :: [Testcase]
  } deriving ( Generic )

-- | The static data that we keep in memory for this subtask
data StaticData = StaticData Template Template Template (Map ByteString Problem)

-- | Read the static data for this subtask from disk
readStaticData :: Config -> IO StaticData
readStaticData cfg = do
  -- Read the description
  desc <- readTemplate cfg "3d/3d.md"
  ex <- readTemplate cfg "3d/3d-example.md"
  backdoor <- readTemplate cfg "3d/backdoor.md"

  -- And read all the problems
  let baseDir = staticFileDir cfg </> "3d"
  pconf <- decodeFileThrow (baseDir </> "config.yaml")
  ps <- forM (Map.toList pconf) $ \(nm, pc) -> do
    -- Read the problem from file
    tp <- readTemplate cfg ("3d" </> description pc)
    let dsamples = readCases $ samples pc
    let pdesc = renderTemplate tp $ Map.singleton ("examples" :: String) dsamples
    pure (BS8.pack nm, Problem
      { pDescription = pdesc
      , pSamples     = dsamples
      , pSecret      = readCases $ secret pc
      })
  pure $ StaticData desc ex backdoor $ Map.fromList ps

-- | Read a testcase 'file', which contains a case per line
readCases :: String -> [Testcase]
readCases = map (toC . words) . lines where
  toC [a,mbB,ans] = Testcase
    { tA = read a
    , tB = if mbB == "x" then Nothing else Just (read mbB)
    , tAns = read ans
    }
  toC _ = error "ThreeD.readCases: invalid line"
