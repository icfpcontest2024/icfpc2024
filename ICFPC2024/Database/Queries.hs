{-# LANGUAGE TypeApplications #-}
module ICFPC2024.Database.Queries (
  registerTeam,
  login,
  getTeam,
  getTeamByEmail,
  authenticate,
  updateTeam,
  updatePassword,
  refreshAPIToken,
  getSubmission,
  getSubmissionByUuid,
  getSubmitHistory,
  storeSubmission,
  storeSubmissionResult,
  setScore,
  getBestScores,
  getAllScores,
  unlockSubtask,
  checkSubtaskUnlocked,
  getSubtasksUnlockedDB,
  countProblemsSolved,
  updateSubtaskScoreboard,
  updateScoreboard,
  getSubtaskId,
  getScoreboardDataGlobal,
  getScoreboardDataSubtask,
  extractSubtaskName,
  saveCodeSubmission,
) where

import ICFPC2024.Environment
import ICFPC2024.Database.Table

import Data.Char ( isDigit )
import Data.Time ( getCurrentTime )
import Data.Maybe ( fromMaybe )
import Data.Password.Bcrypt ( Password, hashPassword, checkPassword, PasswordCheck(..) )
import Data.UUID.V4 ( nextRandom )
import qualified Data.ByteString.Char8 as BS8
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Binary.Builder ( Builder )
import Data.Text ( Text )

import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal ( SqlSelect )
#ifdef PRODUCTION
import Database.MySQL.Connection ( ERRException(..) )
import Database.MySQL.Protocol.Packet ( ERR(..) )
import Database.Persist.MySQL ( insertOnDuplicateKeyUpdate )
import Control.Monad.Catch ( catch, throwM )
#endif

import Control.Arrow ( (***) )
import Control.Monad ( void, forM_, when )
import Control.Monad.Trans ( lift )
import Control.Monad.Reader ( ReaderT )

type QueryM a = ReaderT SqlBackend IO a

-- | Run a query in our server environment
runQuery :: QueryM a -> ServerM dt a
runQuery q = do
  pool <- getDBPool
  lift $ runSqlPool q pool

-- | Register a new team
registerTeam :: Teamname -> Email -> Password -> ServerM dt (Either Builder APIToken)
registerTeam nm em pw = do
  token <- lift $ APIToken <$> nextRandom
  hash <- hashPassword pw
  ( do
      runQuery $ insert_ $ Team nm em hash token
      pure $ Right token
    )
#ifdef PRODUCTION
    `catch` -- catch specific MySQL duplicate key error
    (\(ERRException e) ->
       if errCode e == 1062
       then pure $ Left "Team with this email already exists"
       else throwM $ ERRException e
    )
#endif

-- | Log in to a team, which simply provides the APIToken as proof of login,
--   or 'Nothing' in case of username or password failure
login :: Email -> Password -> ServerM dt (Maybe APIToken)
login em pw = do
  mbTeam <- runQuery $ selectOne $ do
    team <- from $ table @Team
    where_ $ team ^. TeamEmail ==. val em
    pure (team ^. TeamPasswordHash, team ^. TeamApiToken)
  pure $ case mbTeam of
    Nothing -> Nothing
    Just (Value hash, Value token) -> case checkPassword pw hash of
      PasswordCheckFail    -> Nothing
      PasswordCheckSuccess -> Just token

-- | Get the team details based on their API token
getTeam :: TeamId -> ServerM dt (Teamname, Email)
getTeam tid = fmap (unValue *** unValue) $ runQuery $ selectOneUnsafe $ do
  team <- from $ table @Team
  where_ $ team ^. TeamId ==. val tid
  pure (team ^. TeamName, team ^. TeamEmail)


-- | Get the team id based on their email address
getTeamByEmail :: Email -> ServerM dt (Maybe TeamId)
getTeamByEmail em = fmap (fmap unValue) $ runQuery $ selectOne $ do
  team <- from $ table @Team
  where_ $ team ^. TeamEmail ==. val em
  pure $ team ^. TeamId

-- | Authenticate a team based on their API token
authenticate :: APIToken -> ServerM dt (Maybe TeamId)
authenticate token = fmap (fmap unValue) $ runQuery $ selectOne $ do
  team <- from $ table @Team
  where_ $ team ^. TeamApiToken ==. val token
  pure $ team ^. TeamId

-- | Update the team details 
updateTeam :: TeamId -> Teamname -> Email -> ServerM dt ()
updateTeam tid nm em = runQuery $ update $ \team -> do
  set team [ TeamName =. val nm
           , TeamEmail =. val em
           ]
  where_ $ team^.TeamId ==. val tid

-- | Update the team password
updatePassword :: TeamId -> Password -> ServerM dt ()
updatePassword tid pw = hashPassword pw >>= \hash -> runQuery $ update $ \team -> do
  set team [ TeamPasswordHash =. val hash ]
  where_ $ team^.TeamId ==. val tid

-- | Generate fresh api token for a team
refreshAPIToken :: APIToken -> ServerM dt (Maybe APIToken)
refreshAPIToken oldToken = do
  newToken <- lift $ APIToken <$> nextRandom
  cnt <- runQuery $ updateCount $ \team -> do
    set team [ TeamApiToken =. val newToken ]
    where_ $ team^.TeamApiToken ==. val oldToken
  pure $ if cnt > 0 then Just newToken else Nothing

-- | Get a submission from the database
getSubmission :: SubmissionId -> ServerM dt (Maybe Submission)
getSubmission sid = runQuery $ get sid

-- | Get a submission from the database by uuid
getSubmissionByUuid :: TeamId -> SubmissionUUID -> ServerM dt (Maybe Submission)
getSubmissionByUuid tid uuid = fmap (fmap entityVal) $ runQuery $ selectOne $ do
  submit <- from $ table @Submission
  where_ $ submit^.SubmissionTeamId ==. val tid
  where_ $ submit^.SubmissionUuid ==. val uuid
  pure submit

-- | Add a submissions to the system
storeSubmission :: TeamId -> MessageHash -> ServerM dt SubmissionId
storeSubmission tid hash = do
  uuid <- SubmissionUUID <$> lift nextRandom
  now <- lift getCurrentTime
  runQuery $ insert $ Submission now uuid tid hash Nothing

-- | Get the communication history for a team
getSubmitHistory :: TeamId -> Maybe Int -> ServerM dt [Submission]
getSubmitHistory tid mbPage = fmap (map entityVal) $ runQuery $ select $ do
  let perPage = 100
  submit <- from $ table @Submission
  where_ $ submit^.SubmissionTeamId ==. val tid
  orderBy [ desc $ submit^.SubmissionId ]
  forM_ mbPage $ \p ->
    when (p > 1) $ offset $ (fromIntegral p - 1) * perPage
  limit perPage
  pure submit

-- | Store the result of a submission, which is a hash of the response message
storeSubmissionResult :: SubmissionId -> MessageHash -> ServerM dt ()
storeSubmissionResult sid hash = runQuery $ update $ \sub -> do
  set sub [ SubmissionResultHash =. val (Just hash) ]
  where_ $ sub^.SubmissionId ==. val sid

-- | Set the score for a submission/problem
setScore :: SubmissionId -> Problemname -> Score -> ServerM dt ()
setScore sid pnm score = runQuery $ do
  pid <- ensureProblemId pnm
  insert_ $ ScoreRow sid pid score

-- | Let a team unlock a subtask
unlockSubtask :: TeamId -> Subtaskname -> ServerM dt ()
unlockSubtask tid subtask = runQuery $ do
  sid <- ensureSubtaskId subtask
#ifdef PRODUCTION
  insertOnDuplicateKeyUpdate (SubtaskUnlocked tid sid) []
#else
  void $ insertUnique_ (SubtaskUnlocked tid sid)
#endif

-- | Check if a team has unlocked a subtask
checkSubtaskUnlocked :: TeamId -> Subtaskname -> ServerM dt Bool
checkSubtaskUnlocked tid st = fmap unValue $ runQuery $ selectOneUnsafe $ pure $ exists $ do
  (unl :& subtask) <-
    from $ table @SubtaskUnlocked
    `innerJoin` table @Subtask
    `on` (\(unl :& subtask) -> unl^.SubtaskUnlockedSubtaskId ==. subtask^.SubtaskId)
  where_ $ unl^.SubtaskUnlockedTeamId ==. val tid
  where_ $ subtask^.SubtaskName ==. val st


-- | Get all subtasks that the given team has unlocked.
getSubtasksUnlockedDB :: TeamId -> ServerM dt [Subtaskname]
getSubtasksUnlockedDB tid = fmap (fmap unValue) $ runQuery $ select $ do
  (unl :& subtask) <-
    from $ table @SubtaskUnlocked
    `innerJoin` table @Subtask
    `on` (\(unl :& subtask) -> unl^.SubtaskUnlockedSubtaskId ==. subtask^.SubtaskId)
  where_ $ unl^.SubtaskUnlockedTeamId ==. val tid
  orderBy [ asc $ subtask^.SubtaskId ] -- order consistently
  pure $ subtask^.SubtaskName

-- | Count the amount of problems solved for a given subtask and team
countProblemsSolved :: TeamId -> Subtaskname -> ServerM dt Int
countProblemsSolved tid nm = fmap unValue $ runQuery $ selectOneUnsafe $ do
  (sb :& _ :& subtask) <-
    from $ table @ScoreBest
    `innerJoin` table @Problem
    `on` (\(sb :& problem) -> sb^.ScoreBestProblemId ==. problem^.ProblemId)
    `innerJoin` table @Subtask
    `on` (\(_ :& problem :& subtask) -> problem^.ProblemSubtaskId ==. subtask^.SubtaskId )
  where_ $ sb^.ScoreBestTeamId ==. val tid
  where_ $ subtask^.SubtaskName ==. val nm
  pure countRows

-- | Get the best (lowest) scores for all problems of a subtask.
getBestScores :: Subtaskname -> Maybe TeamId -> ServerM dt (Map Problemname Score)
getBestScores task mbTeam = fmap (Map.fromList . map (\(Value p, Value s) -> (p, fromMaybe (error "Score cannot be null") s))) $
  runQuery $ select $ do
    -- Join the scores table with problem and subtask to select only the right scores
    (score :& problem :& subtask) <-
      from $ table @ScoreBest
      `innerJoin` table @Problem
      `on` (\(score :& problem) -> problem^.ProblemId ==. score^.ScoreBestProblemId)
      `innerJoin` table @Subtask
      `on` (\(_ :& problem :& subtask) -> subtask^.SubtaskId ==. problem^.ProblemSubtaskId)
    -- Filter the subtask
    where_ $ subtask^.SubtaskName ==. val task
    -- When needed, filter by team
    forM_ mbTeam $ \team -> do
      where_ $ score^.ScoreBestTeamId ==. val team
    -- And get the minimum score per problem
    groupBy $ problem^.ProblemId
    pure (problem^.ProblemName, min_ (score^.ScoreBestScore))

-- | Get a list of scores of all teams, for scoreboard computation
getAllScores :: ServerM dt [(SubtaskId, ProblemId, TeamId, Score)]
getAllScores = fmap (fmap $ \(Value a, Value b, Value c, Value d) -> (a,b,c,d)) $
  runQuery $ select $ do
    -- Join the scores table with problem and subtask to select only the right scores
    (score :& problem) <-
      from $ table @ScoreBest
      `innerJoin` table @Problem
      `on` (\(score :& problem) -> problem^.ProblemId ==. score^.ScoreBestProblemId)
    pure (problem^.ProblemSubtaskId, problem^.ProblemId, score^.ScoreBestTeamId, score^.ScoreBestScore)

-- | Save a subtask scoreboard
updateSubtaskScoreboard :: SubtaskId -> [(TeamId,Rank)] -> ServerM dt ()
updateSubtaskScoreboard task rnks = runQuery $ do
  -- In one transaction, delete all rows and insert them all
  delete $ from (table @ScoreboardSubtask) >>= \t ->
    where_ $ t^.ScoreboardSubtaskSubtaskId ==. val task
  insertMany_ [ ScoreboardSubtask task tid rank | (tid,rank) <- rnks ]

-- | Save the global scoreboard
updateScoreboard :: [(TeamId,Rank)] -> ServerM dt ()
updateScoreboard rnks = runQuery $ do
  -- In one transaction, delete all rows and insert them all
  delete $ void $ from (table @ScoreboardGlobal)
  insertMany_ [ ScoreboardGlobal tid rank | (tid,rank) <- rnks ]

-- | Get full list of teams, for the scoreboard
getAllTeams :: QueryM [(TeamId, Teamname)]
getAllTeams = fmap (map $ unValue *** unValue) $ select $ do
  team <- from $ table @Team
  pure (team ^. TeamId, team ^. TeamName)

-- | Get the ranks for the subtasks. We return subtaskid here instead of
--   the name to save some bytes, this is more convenient for grouping, we
--   let Haskell do the lookups
getSubtaskScoreboards :: QueryM [(SubtaskId, Rank, TeamId)]
getSubtaskScoreboards = fmap (map $ \(Value a, Value b, Value c) -> (a,b,c)) $ select $ do
  sc <- from $ table @ScoreboardSubtask
  pure (sc ^. ScoreboardSubtaskSubtaskId, sc ^. ScoreboardSubtaskRank, sc ^. ScoreboardSubtaskTeamId)

-- | Similar to 'getSubtaskScoreboards', but then for the problem of one subtask
getProblemScoreboards :: SubtaskId -> QueryM [(ProblemId, Score, TeamId)]
getProblemScoreboards stid = fmap (map $ \(Value a, Value b, Value c) -> (a, b,c)) $ select $ do
  (sc :& problem) <-
    from $ table @ScoreBest
    `innerJoin` table @Problem
    `on` (\(sc :& problem) -> problem^.ProblemId ==. sc^.ScoreBestProblemId)
  where_ $ problem ^. ProblemSubtaskId ==. val stid
  pure (sc ^. ScoreBestProblemId, sc ^. ScoreBestScore, sc ^. ScoreBestTeamId)

-- | Get the list of subtasks, for the scoreboard, ordered by id (and thus by 'discovery time')
getAllSubtasks :: QueryM [(SubtaskId, Subtaskname)]
getAllSubtasks = fmap (map $ unValue *** unValue) $ select $ do
  task <- from $ table @Subtask
  orderBy [ asc $ task ^. SubtaskId ]
  pure (task ^. SubtaskId, task ^. SubtaskName)

-- | Get the list of problems for a subtask, for the scoreboard, ordered by name
getSubtaskProblems :: SubtaskId -> QueryM [(ProblemId, Problemname)]
getSubtaskProblems stid = fmap (map $ unValue *** unValue) $ select $ do
  problem <- from $ table @Problem
  where_ $ problem ^. ProblemSubtaskId ==. val stid
  orderBy [ asc @Int $ length_ $ problem ^. ProblemName, asc $ problem ^. ProblemName ]
  pure (problem ^. ProblemId, problem ^. ProblemName)

-- | Get the ranks for the global scoreboard, sorted by rank
getGlobalScoreboard :: QueryM [(Rank, TeamId)]
getGlobalScoreboard = fmap (map $ unValue *** unValue) $ select $ do
  sc <- from $ table @ScoreboardGlobal
  orderBy [ asc $ sc ^. ScoreboardGlobalRank ]
  pure (sc ^. ScoreboardGlobalRank, sc ^. ScoreboardGlobalTeamId)

-- | Get the scoreboard for a single subtask, sorted by rank
getSubtaskScoreboard :: SubtaskId -> QueryM [(Rank, TeamId)]
getSubtaskScoreboard stid = fmap (map $ unValue *** unValue) $ select $ do
  sc <- from $ table @ScoreboardSubtask
  where_$ sc ^. ScoreboardSubtaskSubtaskId ==. val stid
  orderBy [ asc $ sc ^. ScoreboardSubtaskRank ]
  pure (sc ^. ScoreboardSubtaskRank, sc ^. ScoreboardSubtaskTeamId)

-- | Get the subtask id, returning 'Nothing' when the subtask is not in de database (yet,
--   because they are added on demand whenever somebody gets a finite score)
getSubtaskId :: Subtaskname -> ServerM dt (Maybe SubtaskId)
getSubtaskId nm = runQuery $ fmap (fmap unValue) $ selectOne $ do
  st <- from $ table @Subtask
  where_ $ st^.SubtaskName ==. val nm
  pure $ st^.SubtaskId

-- | Get all data for the scoreboard. We run this in a single transaction for consistency,
--   but we request the data separately and let Haskell do the grouping and such
getScoreboardDataGlobal :: ServerM dt
  ( [(TeamId, Teamname)]
  , [(SubtaskId, Subtaskname)]
  , [(Rank, TeamId)]
  , [(SubtaskId, Rank, TeamId)]
  )
getScoreboardDataGlobal = runQuery $ do
  global <- getGlobalScoreboard
  local <- getSubtaskScoreboards
  teams <- getAllTeams
  subtasks <- getAllSubtasks
  pure (teams, subtasks, global, local)

-- | Similar to 'getScoreboardDataGlobal', but then for a given subtask
getScoreboardDataSubtask :: SubtaskId -> ServerM dt
  ( [(TeamId, Teamname)]
  , [(ProblemId, Problemname)]
  , [(Rank, TeamId)]
  , [(ProblemId, Score, TeamId)]
  )
getScoreboardDataSubtask stid = runQuery $ do
  ranks <- getSubtaskScoreboard stid
  teams <- getAllTeams
  problems <- getSubtaskProblems stid
  perProblem <- getProblemScoreboards stid
  return (teams, problems, ranks, perProblem)

-- | Save a final code submission (a bit untyped, but we just dump text to the DB
--   without doing anything with that in the code)
saveCodeSubmission :: TeamId -> Text -> Text -> Text -> ServerM dt ()
saveCodeSubmission tid url lang jury = do
  now <- lift getCurrentTime
  runQuery $ insert_ $ CodeSubmission tid now url lang jury

-- | Get the problem id of a problem, and insert when it doesn't exist
ensureProblemId :: Problemname -> QueryM ProblemId
ensureProblemId nm = do
  mbPid <- selectOne $ do
    problem <- from $ table @Problem
    where_ $ problem^.ProblemName ==. val nm
    pure $ problem^.ProblemId
  case mbPid of
    Just (Value pid) -> pure pid
    Nothing -> do
      task <- ensureSubtaskId $ extractSubtaskName nm
      insert $ Problem nm task

-- | Get the id of a subtask, and insert when it doesn't exist
ensureSubtaskId :: Subtaskname -> QueryM SubtaskId
ensureSubtaskId nm = do
  mbPid <- selectOne $ do
    task <- from $ table @Subtask
    where_ $ task^.SubtaskName ==. val nm
    pure $ task^.SubtaskId
  case mbPid of
    Just (Value pid) -> pure pid
    Nothing -> insert $ Subtask nm

-- | Extract the subtask name from the problemname we assume the format is always subtaskname
--   followed by some digits (but 3d also starts with a digit, so we strip from the end)
extractSubtaskName :: Problemname -> Subtaskname
extractSubtaskName nm = Subtaskname $ BS8.dropWhileEnd isDigit $ unProblemname nm

-- | Select a record when we are sure we get a result, e.g. when we already have a primary key
selectOneUnsafe :: SqlSelect a b => SqlQuery a -> QueryM b
selectOneUnsafe q = selectOne q >>= \case
  Nothing -> error "selectOneUnsafe: impossible"
  Just r  -> pure r
