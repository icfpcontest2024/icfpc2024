module ICFPC2024.Puzzles.Lambdaman (
  handleRequest,
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Environment ( ServerM, getStaticData, logM, LogPath (..), toLogStr )
import ICFPC2024.Database ( SubmissionId, TeamId, Problemname (..), Score )
import ICFPC2024.Scoring ( storeScore, checkEnrolled )
import ICFPC2024.Puzzles.Helpers ( getProblemInfo )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )
import ICFPC2024.Puzzles.Static ( staticLambdaman )
import ICFPC2024.Puzzles.Lambdaman.Static ( StaticData (..), Problem (..) )

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Handle an incoming ICFP request, or return Nothing when the request is not part of this subtask
handleRequest :: SubmissionId -> TeamId -> (Score, ByteString) -> ServerM 'True (Maybe Term)
handleRequest sid tid (score, s)
  -- Getting the subtask description
  | s == "get lambdaman" = checkEnrolled tid "lambdaman" $ do
      StaticData desc mp <- staticLambdaman . fst <$> getStaticData
      ts <- getProblemInfo "lambdaman" tid (map (Problemname . BS8.toStrict) $ Map.keys mp)
      pure $ Just $ renderTemplate desc ts
  -- Getting a problem
  | "get lambdaman" `BS8.isPrefixOf` s = checkEnrolled tid "lambdaman" $ do
      StaticData _ mp <- staticLambdaman . fst <$> getStaticData
      pure $ fmap pExpression $ BS8.drop 4 s `Map.lookup` mp
  -- Solving the problem
  | "solve lambdaman" `BS8.isPrefixOf` s
  , ["solve",nm,mvs] <- BS8.words s = checkEnrolled tid "lambdaman" $ do
      StaticData _ mp <- staticLambdaman . fst <$> getStaticData
      case nm `Map.lookup` mp of
        Nothing -> pure Nothing
        Just prob ->
          if BS8.length mvs > 1_000_000
          then do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " has more than 1,000,000 characters"
            pure $ Just $ TString "There can be at most 1,000,000 characters"
          else if not (BS8.all (\x -> x `elem` directions) mvs)
          then do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " has invalid characters"
            pure $ Just $ TString "The solution must consist of U, R, D and L characters only"
          else if validateSolution prob mvs
          then do
            -- Solved! So store the score
            Just <$> storeScore sid tid (Problemname $ BS8.toStrict nm) (Just score)
            -- Wrong answer
          else do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " was wrong"
            pure $ Just $ TString $ "Your solution for " <> nm <> " was wrong."
  | otherwise = pure Nothing


-- | Simulate the path, and check that all positions where visited
validateSolution :: Problem -> ByteString -> Bool
validateSolution pr moves = ret where
  begin = pBegin pr
  mp = pGrid pr
  lu r c = Map.findWithDefault '#' (r,c) mp
  (allVis, _) = foldl f (Set.singleton begin, begin) $ BS8.unpack moves
  f (vis,p@(r,c)) ch =
    let r' = r + dr ch
        c' = c + dc ch
        p' = (r', c')
        nc = lu r' c'
    in if nc == '#'
       then (vis,p)
       else (Set.insert p' vis, p')
  ret = null [ p | (p,'.') <- Map.toList mp, not (p `Set.member` allVis) ]

directions :: String
directions = "URDL"

dr :: Char -> Int
dr 'U' = -1
dr 'R' = 0
dr 'D' = 1
dr 'L' = 0
dr _   = error "Invalid move"

dc :: Char -> Int
dc 'U' = 0
dc 'R' = 1
dc 'D' = 0
dc 'L' = -1
dc _ = error "Invalid move"
