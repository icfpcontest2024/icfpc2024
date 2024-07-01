module ICFPC2024.Puzzles.Efficiency (
  handleRequest,
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Environment ( ServerM, getStaticData, logM, LogPath (..), toLogStr )
import ICFPC2024.Database
import ICFPC2024.Scoring ( storeScore, checkEnrolled )
import ICFPC2024.Puzzles.Helpers ( getProblemInfo )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )
import ICFPC2024.Puzzles.Static ( staticEfficiency )
import ICFPC2024.Puzzles.Efficiency.Static

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map

-- | Handle an incoming ICFP request, or return Nothing when the request is not part of this subtask
handleRequest :: SubmissionId -> TeamId -> ByteString -> ServerM 'True (Maybe Term)
handleRequest sid tid s
  -- Backdoor, no enroll check
  | s == "get /etc/passwd" = do
      StaticData _ pwd _ _ <- staticEfficiency . fst <$> getStaticData
      pure $ Just $ renderTemplate pwd ()
  | s == "login headmaster lambda" = do
      StaticData _ _ backdoor _ <- staticEfficiency . fst <$> getStaticData
      logM Puzzles $ "Team " <> toLogStr tid <> " found efficiency backdoor"
      unlockSubtask tid "efficiency"
      pure $ Just $ renderTemplate backdoor ()
  | "login " `BS8.isPrefixOf` s
  , ["login",_,_] <- BS8.words s = do
      pure $ Just $ TString "Username/password incorrect"
  -- Getting the subtask description
  | s == "get efficiency" = checkEnrolled tid "efficiency" $ do
      StaticData desc _ _ mp <- staticEfficiency . fst <$> getStaticData
      ts <- getProblemInfo "efficiency" tid (map (Problemname . BS8.toStrict) $ Map.keys mp)
      pure $ Just $ renderTemplate desc ts
  -- Getting a problem
  | "get efficiency" `BS8.isPrefixOf` s = checkEnrolled tid "efficiency" $ do
      StaticData _ _ _ mp <- staticEfficiency . fst <$> getStaticData
      pure $ fmap pExpression $ BS8.drop 4 s `Map.lookup` mp
  -- Solving the problem
  | "solve efficiency" `BS8.isPrefixOf` s
  , ["solve",nm,ans] <- BS8.words s = checkEnrolled tid "efficiency" $ do
      StaticData _ _ _ mp <- staticEfficiency . fst <$> getStaticData
      case nm `Map.lookup` mp of
        Nothing -> pure Nothing
        Just prob ->
          -- Check the answer as a string
          if BS8.pack (show $ pAnswer prob) == ans
          then do
            -- Solved! So store the score
            Just <$> storeScore sid tid (Problemname $ BS8.toStrict nm) Nothing
            -- Wrong answer
          else do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " was wrong"
            pure $ Just $ TString $ "Your answer for " <> nm <> " was wrong"
  | otherwise = pure Nothing
