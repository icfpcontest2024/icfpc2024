module ICFPC2024.Puzzles.Helpers (
  getProblemInfo,
  ProblemInfo,
) where

import ICFPC2024.Environment ( ServerM )
import ICFPC2024.Database

import GHC.Generics ( Generic )

import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Map ( Map )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import Text.Mustache ( ToMustache(..), object, (~>), (~=) )

-- | Problem info for use in the mustache templates
data ProblemInfo = ProblemInfo
  { piName      :: Problemname
  , piBestScore :: Maybe Score
  , piYourScore :: Maybe Score
  } deriving ( Generic )

instance ToMustache ProblemInfo where
  toMustache p = object
    [ "name"       ~> decodeUtf8 (unProblemname $ piName p)
    , "best_score" ~= piBestScore p
    , "your_score" ~= piYourScore p
    ]

-- | Get problem info for a subtask for use in the mustache template
getProblemInfo :: Subtaskname -> TeamId -> [Problemname] -> ServerM dt (Map String [ProblemInfo])
getProblemInfo subtask tid ps = do
  -- Get your best scores and global scores
  bestScores <- getBestScores subtask Nothing
  yourScores <- getBestScores subtask (Just tid)
  -- Make the list of probleminfo
  let lst = [ ProblemInfo
              { piName      = pname
              , piBestScore = pname `Map.lookup` bestScores
              , piYourScore = pname `Map.lookup` yourScores
              }
            | pname <- ps
            ]
  -- Sort nicely, so that lambdaman11 does not come before lambdaman2
  let cmp pn = (BS8.length (unProblemname pn), pn)
  let problems = sortBy (comparing $ cmp . piName) lst
  
  -- And put this in a nice map
  pure $ Map.singleton "problems" problems
