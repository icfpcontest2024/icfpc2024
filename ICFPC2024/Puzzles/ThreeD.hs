module ICFPC2024.Puzzles.ThreeD (
  handleRequest,
  validateSolution,
) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Compression ( intCompress )
import ICFPC2024.Environment ( ServerM, getStaticData, logM, LogPath (..), toLogStr )
import ICFPC2024.Database ( SubmissionId, TeamId, Problemname (..), scoreFromIntegral, unlockSubtask )
import ICFPC2024.Scoring ( storeScore, checkEnrolled )
import ICFPC2024.Puzzles.Helpers ( getProblemInfo )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )
import ICFPC2024.Puzzles.Static ( static3D )
import ICFPC2024.Puzzles.ThreeD.Static ( StaticData (..), Problem (..), Testcase (..) )

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

import Text.Read ( readMaybe )

import TTop ( runProgram, Outcome(..), renderOutcome )
import TTop.Board ( parseBoard, formatTextNoComments, Value(I), Board, BoardError(..) )
import TTop.History ( newState, renderState, RuntimeError (..) )

-- | Handle an incoming ICFP request, or return Nothing when the request is not part of this subtask
handleRequest :: SubmissionId -> TeamId -> ByteString -> ServerM 'True (Maybe Term)
handleRequest sid tid s
  -- Getting into the problem via a backdoor
  | s == "get 3d-backdoor" = do -- no enroll check
      logM Puzzles $ "Team " <> toLogStr tid <> " found 3d-backdoor"
      StaticData _ _ bd _ <- static3D . fst <$> getStaticData
      unlockSubtask tid "3d"
      pure $ Just $ renderTemplate bd ()
  -- Getting the subtask description
  | s == "get 3d" = checkEnrolled tid "3d" $ do
      StaticData desc _ _ mp <- static3D . fst <$> getStaticData
      ts <- getProblemInfo "3d" tid (map (Problemname . BSL8.toStrict) $ Map.keys mp)
      pure $ Just $ renderTemplate desc ts
  -- Get the example trace
  | s == "get 3d-example" = checkEnrolled tid "3d" $ do
      StaticData _ ex _ _ <- static3D . fst <$> getStaticData
      pure $ Just $ renderTemplate ex ()
  -- Getting a problem
  | "get 3d" `BSL8.isPrefixOf` s = checkEnrolled tid "3d" $ do
      StaticData _ _ _ mp <- static3D . fst <$> getStaticData
      pure $ fmap pDescription $ BSL8.drop 4 s `Map.lookup` mp
  -- Solving the problem
  | "solve 3d" `BSL8.isPrefixOf` s
  , (firstline:ts) <- BSL8.lines s
  , ["solve",nm] <- BSL8.words firstline = checkEnrolled tid "3d" $ do
      StaticData _ _ _ mp <- static3D . fst <$> getStaticData
      case nm `Map.lookup` mp of
        Nothing -> pure Nothing
        Just prob -> case validateSolution nm prob ts of
          Left e -> do
            logM Puzzles $ toLogStr nm <> " submission " <> toLogStr sid <> " was wrong: " <> toLogStr e
            pure $ Just $ TString $ BSL8.fromStrict e
          Right score -> do
            -- Solved! So store the score
            let sc = scoreFromIntegral score
            Just <$> storeScore sid tid (Problemname $ BSL8.toStrict nm) (Just sc)
  -- Doing a testrun
  | "test 3d" `BSL8.isPrefixOf` s
  , (firstline:ts) <- BSL8.lines s
  , ["test","3d",sa,sb] <- BSL8.words firstline = checkEnrolled tid "3d" $ case pBoard ts of
      Left e -> pure $ Just $ TString $ BSL8.fromStrict e
      Right board -> case readMaybe (BSL8.unpack sa) of
        Nothing -> pure $ Just $ TString "Could not parse value A"
        Just a -> case readMaybe (BSL8.unpack sb) of
          Nothing -> pure $ Just $ TString "Could not parse value B"
          Just b -> do
            let (sts, outcome) = runProgram False (Just 4) $ newState [I a, I b] board
            let retS = BS8.unlines $ map renderState sts ++ renderOutcome outcome
            let cleaned = BS8.map (\x -> if x == '{' then '(' else if x == '}' then ')' else x) retS
            pure $ Just $ intCompress "" $ truncateOutput cleaned

  | otherwise = pure Nothing

-- | Truncate the output of the test command, we can't handle it when it is too big
truncateOutput :: BS8.ByteString -> BS8.ByteString
truncateOutput bs
  | BS8.length bs <= 100_000 = bs
  | otherwise = BS8.take 90_000 bs <> "\n[...truncated...]\n" <> BS8.takeEnd 10_000 bs


pBoard :: [ByteString] -> Either BS8.ByteString Board
pBoard ls = parseBoard formatTextNoComments (BS8.toStrict $ BSL8.unlines ls)

validateSolution :: ByteString -> Problem -> [ByteString] -> Either BS8.ByteString Integer
validateSolution problemName pr ls = do
  board <- pBoard ls

  -- Function for checking a single case
  let checkCase c =
        let inputs = [I (tA c)] ++ [I i | Just i <- [tB c]]
        in case runProgram False Nothing (newState inputs board) of
          (_, Terminated _) -> Left "Your program did not submit an answer"
          (_, Submitted val vol) -> case val of
            I r | tAns c == r -> Right vol
            I r | problemName == "3d12" && abs (tAns c - r) <= 1 -> Right vol
            _ -> Left "Your program submitted the wrong answer"
          (_, Crashed TickLimitExceeded) -> Left "Your program exceeded the tick limit"
          (_, Crashed NoChange) -> Left "Your program terminated without submitting an answer"
          (_, Crashed (ConflictingWarps _)) -> Left "Your program crashed: ConflictingWarps"
          (_, Crashed (TooManySubmits _)) -> Left "Your program crashed: TooManySubmits"
          (_, Crashed (WarpToInvalidDt _)) -> Left "Your program crashed: WarpToInvalidDt"
          (_, Crashed (WarpToConflictingTimes _)) -> Left "Your program crashed: WarpToConflictingTimes"
          (_, Crashed (WarpDtMustBePositive _)) -> Left "Your program crashed: WarpDtMustBePositive"
          (_, Crashed (BoardError ConflictingReductions)) -> Left "Your program crashed: ConflictingReductions"

      -- Then go over all cases
      checkCases [] v = pure v
      checkCases (c:cs) v = do
        v' <- checkCase c
        checkCases cs (v + v')

  -- Then go over all cases
  checkCases (pSamples pr ++ pSecret pr) 0

