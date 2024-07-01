module ICFPC2024.Scoring (
  recomputeScores,
  getScoreboard,
  Scoreboard (..),
  ScoreboardRow (..),
  storeScore,
  checkEnrolled,
  getSubtasksUnlocked,
) where

import ICFPC2024.AST ( Term )
import ICFPC2024.Config ( Config(..), TimeConfig(..) )
import ICFPC2024.Database as DB
import ICFPC2024.Environment ( ServerM, getConfig, getStaticData, LogPath (..), logM, toLogStr )
import ICFPC2024.Puzzles.Static ( staticSolved, staticNotEnrolled )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )

import Data.Time ( getCurrentTime, addUTCTime )
import Data.Maybe ( fromMaybe, catMaybes )
import Data.Ord ( comparing, Down(..) )
import Data.Foldable ( toList )
import Data.List ( sortOn, sortBy, groupBy )
import Data.Aeson ( Value (String, Null), ToJSON (toJSON) )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Control.Monad ( forM )
import Control.Monad.Trans ( lift )

import GHC.Generics ( Generic )

-- * Scoreboard computation

-- | Recompute the scoreboards per subtask and the global scoreboard
recomputeScores :: ServerM dt ()
recomputeScores = do
  -- Get all scores at once, this is |teams * problems|, which should be a couple of thousands at most
  scoreList <- getAllScores
  let perSubtask = Map.unionsWith (Seq.><) $ map (\(s,p,t,i) -> Map.singleton s $ Seq.singleton (p,t,i)) scoreList

  -- Update per subtask
  allRanks <- forM (Map.toList perSubtask) $ \(subt,scs) -> do
    let ranks = bordaCount $ toList scs
    updateSubtaskScoreboard subt ranks
    pure [ (subt,t,r) | (t,r) <- ranks ]

  -- From that, we compute the global scoreboard
  let totalRanks = bordaCount $ concat allRanks
  updateScoreboard totalRanks

-- | Borda counting method, where each problem is a voter and teams are candidates.
--   In other words, the score for each team is the amount of teams that are strictly
--   worse than you. This eliminates the need for putting weights on problems,
--   as this automatically gives you more points for solver problems that less
--   other teams solve.
--   This runs in O(p * t * log(t)) where p = |problems| and t = |teams|
--   <https://en.wikipedia.org/wiki/Borda_count>
bordaCount :: (Ord score, Ord pid) => [(pid, TeamId, score)] -> [(TeamId, Rank)]
bordaCount rs = ranks where
  -- Some global sets, to deal with missing team,problem combinations
  probs  = Set.fromList [ p | (p, _, _) <- rs ]
  teams  = Set.fromList [ t | (_, t, _) <- rs ]
  nteams = Set.size teams

  -- Group per problem
  scorePerP = Map.unionsWith (Seq.><) $ map (\(p,t,i) -> Map.singleton p $ Seq.singleton (t,i)) rs

  -- Borda score per per problem per team
  mkBorda ts = f 0 Map.empty $ groupBy (\(_,s1) (_,s2) -> s1 == s2) $ sortBy (comparing snd) ts where
    f _ mp [] = mp
    f above mp (x:xs) =
      let above' = above + length x
          worse  = nteams - above'
      in f above' (Map.unions [Map.singleton t worse | (t,_) <- x] `Map.union` mp) xs
  scoreBorda = Map.map (mkBorda . toList) scorePerP

  -- Total team score, here higher is better
  teamScore =
    [ (t, s)
    | t <- Set.toList teams
    , let s = sum
            [ ps
            | p <- Set.toList probs
            , let mbS = p `Map.lookup` scoreBorda >>= Map.lookup t
            , let ps = fromMaybe 0 mbS
            ]
    ]
  ranks = toRank (1, 0, Nothing) $ sortBy (comparing $ Down . snd) teamScore

-- | Convert s sorted list of scores to ranks,
-- the tuple is (next rank, rank of previous score, previous score)
toRank :: Eq s => (Int, Int, Maybe s) -> [(TeamId, s)] -> [(TeamId, Rank)]
toRank _ [] = []
toRank (nr,pr,ps) ((t,ts):xs)
  -- Tied with previous team
  | Just ts == ps = (t, Rank pr) : toRank (nr+1, pr, ps) xs
  -- Next rank
  | otherwise = (t, Rank nr) : toRank (nr+1, nr, Just ts) xs


-- * Retrieving the scoreboard

-- | Representation of the scoreboard, we use Aeson values so that cells can
--   contain both strings or numbers, and we can render them nicely in both
--   mustache as well as JSON
data Scoreboard = Scoreboard
  { columns :: [Value]
  , rows :: [ScoreboardRow]
  } deriving ( Generic )

data ScoreboardRow = ScoreboardRow
  { isYou :: Bool
  , values :: [Value]
  } deriving ( Generic )

instance ToJSON Scoreboard
instance ToJSON ScoreboardRow

-- | Get the full scoreboard
getScoreboard :: Maybe TeamId -> Maybe SubtaskId -> ServerM dt Scoreboard
getScoreboard mbTid Nothing     = getScoreboardDataGlobal >>= renderScoreboard mbTid
getScoreboard mbTid (Just stid) = do
  (teams, problems, ranks, perProblem) <- getScoreboardDataSubtask stid
  -- we do not store the ranks for the individual problems, only the
  -- raw scores, so we need to convert them to ranks.
  let problemMap = Map.unionsWith (Seq.><) $ map (\(p,s,t) -> Map.singleton p $ Seq.singleton (t,s)) perProblem
      rankedMap = Map.map (toRank (1, 0, Nothing) . toList . Seq.sortOn snd) problemMap
      perProblemRank =
        [ (p,s,t)
        | (p,rs) <- Map.toList rankedMap
        , (t,s) <- rs
        ]
  renderScoreboard mbTid (teams, problems, ranks, perProblemRank)

-- | Render based on the data, this is a separate function to deal with
-- type differences between the global and subtask scoreboards, but the
-- variable names are as if it is just the global scoreboard.
-- Global is sorted by rank, and subtasks is sorted by id
renderScoreboard :: (ToJSON name, Ord stid) => Maybe TeamId ->
  ( [(TeamId, Teamname)]
  , [(stid, name)]
  , [(Rank, TeamId)]
  , [(stid, Rank, TeamId)]
  ) -> ServerM dt Scoreboard
renderScoreboard mbTid (teams, subtasks, global, local) = do
  let teamMap = Map.fromList teams
  let localMap = Map.fromList [ ((stid,tid),rank) | (stid,rank,tid) <- local ]
  let cols = [ String "#", String "team" ] ++ map (toJSON . snd) subtasks
  let nextRank = toJSON $ length global + 1
  let globalTeams = Set.fromList $ map snd global
  let rws =
        -- Teams that got a rank on the global scoreboard
        [ ScoreboardRow (Just tid == mbTid) $
           [ toJSON rank
           , maybe Null toJSON $ tid `Map.lookup` teamMap
           ] ++
           [ maybe Null toJSON $ (stid,tid) `Map.lookup` localMap
           | (stid,_) <- subtasks
           ]
        | (rank,tid) <- global
        ] ++
        -- Teams that did not score points yet
        [ ScoreboardRow (Just tid == mbTid) $ [ nextRank, toJSON nm ] ++ [ Null | _ <- subtasks ]
        | (tid,nm) <- teams
        , not $ tid `Set.member` globalTeams
        ]
  -- Now check if we're in the freeze, and if so, we remove some info from the scoreboard
  tims <- contestTimes <$> getConfig
  now <- lift getCurrentTime
  -- We freeze around the end of the lightning round, and the real contest
  let start1 = addUTCTime (fromIntegral $ (-3600) * freezeMarginHour tims) $ lightningEnd tims
  let end1   = addUTCTime (fromIntegral $    3600 * freezeMarginHour tims) $ lightningEnd tims
  let start2 = addUTCTime (fromIntegral $ (-3600) * freezeMarginHour tims) $ contestEnd tims
  let inFreeze = (start1 <= now && now <= end1) || start2 <= now -- we never unfreeze after the contest :-)
  let rws' = if not inFreeze then rws else
        let top10 = [ ScoreboardRow y $ String "?" : tn : [String "?" | _ <- xs]
                    | ScoreboardRow y (_:tn:xs) <- take 10 rws ]
            rest = drop 10 rws
            -- sort the top 10 alphabetically
        in  sortOn values top10 ++ rest
  -- And return
  pure $ Scoreboard
    { columns = cols
    , rows    = rws'
    }

-- * Problem unlocking

-- | Hardcoded unlock order, per subtask we specify
--   how many problems per other subtask must be solved.
--   "hello" does not need to be unlocked.
unlockOrder :: [ (Subtaskname, [(Subtaskname, Int)]) ]
unlockOrder =
  [ ("lambdaman",  [("hello", 2)])
  , ("spaceship",  [("hello", 2)])
  , ("3d",         [("lambdaman", 5), ("spaceship", 5)])
  , ("efficiency", [("3d", 5)])
  ]

-- | Store the score for a submission/problem. This also triggers problem unlocking,
--   and returns the message that the user should see.
storeScore :: SubmissionId -> TeamId -> Problemname -> Maybe Score -> ServerM 'True Term
storeScore sid tid nm mbSc = do
  -- For binary problems, we score 100 points (it does not matter...)
  let sc = fromMaybe (Score 100) mbSc

  -- Save the score to the DB
  setScore sid nm sc

  -- Logging
  logM Puzzles $ "Submission " <> toLogStr sid <> " scored " <> toLogStr sc <> " for " <> toLogStr nm

  -- Check if we unlocked some subtask we did not unlock so far
  -- Only look at the subtask we just scored some points for
  let subtask = extractSubtaskName nm
  unlocked <- getSubtasksUnlocked tid
  let mbUnlock =
        [ (st, deps)
        | (st, deps) <- unlockOrder
        , st `notElem` unlocked -- not unlocked yet
        , subtask `elem` map fst deps -- the problem we just solved may help
        ]

  -- Count if we satisfy the constraints. This may do some duplicate
  -- queries in the beginning of the contest (when scoring the first hello
  -- problem), but it's all small data so that should be fine.
  newUnlocked <- fmap catMaybes $ forM mbUnlock $ \(st,deps) -> do
    -- Count for all dependencies, we recurse so that we
    -- shortcut on the first false
    let f [] = do -- No more dependencies, so we unlock it
          unlockSubtask tid st
          pure $ Just st
        f ((dep,dcnt):ds) = do -- Check this dependency
          cnt <- countProblemsSolved tid dep
          if cnt < dcnt
            then pure Nothing
            else f ds
    f deps

  -- Construct the return message
  solvedT <- staticSolved . fst <$> getStaticData
  let mp :: Map.Map String Value
      mp = Map.fromList [ ("name", toJSON nm)
                        , ("score", toJSON mbSc)
                        , ("unlocked", toJSON newUnlocked)
                        ]
  pure $ renderTemplate solvedT mp

-- | Get all subtasks a team can currently access
getSubtasksUnlocked :: TeamId -> ServerM dt [Subtaskname]
getSubtasksUnlocked tid = do
  timeUnlock <- lightningEnd . contestTimes <$> getConfig
  now <- lift getCurrentTime
  if now >= timeUnlock -- After the lightning ends, everybody unlocks everything
    then pure $ map fst unlockOrder
    else getSubtasksUnlockedDB tid

-- | Verify that the team is enrolled in this subtask, run the continuation
--   in that case, otherwise return a nice error message
checkEnrolled :: TeamId -> Subtaskname -> ServerM 'True (Maybe Term) -> ServerM 'True (Maybe Term)
checkEnrolled tid subtask cont = do
  timeUnlock <- lightningEnd . contestTimes <$> getConfig
  now <- lift getCurrentTime
  if now >= timeUnlock -- After the lightning ends, everybody unlocks everything
    then cont
    else checkSubtaskUnlocked tid subtask >>= \case
    True -> cont
    False -> do
      templ <- staticNotEnrolled . fst <$> getStaticData
      let mp :: Map.Map String Value
          mp = Map.fromList [ ("name", toJSON subtask) ]
      pure $ Just $ renderTemplate templ mp
