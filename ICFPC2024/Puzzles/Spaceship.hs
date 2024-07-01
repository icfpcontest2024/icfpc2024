module ICFPC2024.Puzzles.Spaceship (
  handleRequest,
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Environment ( ServerM, getStaticData, logM, LogPath (..), toLogStr )
import ICFPC2024.Scoring ( storeScore, checkEnrolled )
import ICFPC2024.Database ( SubmissionId, TeamId, Problemname (..), scoreFromIntegral )
import ICFPC2024.Puzzles.Helpers ( getProblemInfo )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )
import ICFPC2024.Puzzles.Static ( staticSpaceship )
import ICFPC2024.Puzzles.Spaceship.Static

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Handle an incoming ICFP request, or return Nothing when the request is not part of this subtask
handleRequest :: SubmissionId -> TeamId -> ByteString -> ServerM 'True (Maybe Term)
handleRequest sid tid s
  -- Getting the subtask description
  | s == "get spaceship" = checkEnrolled tid "spaceship" $ do
      StaticData desc mp <- staticSpaceship . fst <$> getStaticData
      ts <- getProblemInfo "spaceship" tid (map (Problemname . BS8.toStrict) $ Map.keys mp)
      pure $ Just $ renderTemplate desc ts
  -- Getting a problem
  | "get spaceship" `BS8.isPrefixOf` s = checkEnrolled tid "spaceship" $ do
      StaticData _ mp <- staticSpaceship . fst <$> getStaticData
      pure $ fmap expression $ BS8.drop 4 s `Map.lookup` mp
  -- Solving the problem
  | "solve spaceship" `BS8.isPrefixOf` s
  , ["solve",nm,mvs] <- BS8.words s = checkEnrolled tid "spaceship" $ do
      StaticData _ mp <- staticSpaceship . fst <$> getStaticData
      case nm `Map.lookup` mp of
        Nothing -> pure Nothing
        Just prob ->
          -- Check validity
          if BS8.length mvs > 10_000_000
          then do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " contains more than 10,000,000 characters"
            pure $ Just $ TString "There can be at most 10,000,000 characters"
          else if not (BS8.all (\c -> '1' <= c && c <= '9') mvs)
          then do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " contains wrong characters"
            pure $ Just $ TString "The solution must consist of characters 1..9 only"
          -- Check the answer as a string
          else if validate prob mvs
          then do
            -- Solved! So store the score
            let score = scoreFromIntegral $ BS8.length mvs
            Just <$> storeScore sid tid (Problemname $ BS8.toStrict nm) (Just score)
            -- Wrong answer
          else do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " was wrong"
            pure $ Just $ TString $ "Your answer for " <> nm <> " was wrong"
  | otherwise = pure Nothing


-- * Validating a solution

validate :: Problem -> ByteString -> Bool
validate prob mvs = ok where
  simulate p@(x,y) (sx,sy) z = case BS8.uncons z of
    Nothing     -> Set.singleton p
    Just (c,zs) -> Set.insert p $ simulate (x+sx+dx c,y+sy+dy c) (dx c+sx,dy c+sy) zs
  visited = simulate (0,0) (0,0) mvs
  ok = all (`Set.member` visited) (targets prob)

dx :: Char -> Integer
dx '1' = -1
dx '2' = 0
dx '3' = 1
dx '4' = -1
dx '5' = 0
dx '6' = 1
dx '7' = -1
dx '8' = 0
dx '9' = 1
dx _ = error "dx: impossible"

dy :: Char -> Integer
dy '1' = -1
dy '2' = -1
dy '3' = -1
dy '4' = 0
dy '5' = 0
dy '6' = 0
dy '7' = 1
dy '8' = 1
dy '9' = 1
dy _ = error "dx: impossible"
