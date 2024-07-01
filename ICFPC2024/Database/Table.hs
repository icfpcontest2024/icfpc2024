{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ICFPC2024.Database.Table where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Esqueleto.Experimental ( SqlString )

import Data.Functor ( (<&>) )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time ( UTCTime )
import Data.UUID ( UUID )
import Data.String ( IsString )
import Data.Digest.Pure.MD5 ( MD5Digest )
import Data.Binary as B ( Binary (..), encode, decode )
import Data.ByteString.Char8 ( ByteString, toStrict, fromStrict )
import Data.Password.Bcrypt ( PasswordHash, Bcrypt )
import Data.Password.Instances ()
import Data.Aeson ( FromJSON, ToJSON (..), Value(String) )
import Data.Word ( Word64 )

import Web.HttpApiData ( FromHttpApiData (..) )

import System.Log.FastLogger ( ToLogStr (..) )

-- * Newtypes to make the DB layer strongly typed

newtype Teamname = Teamname { unTeamname :: Text }
  deriving newtype ( PersistField, ToJSON, FromJSON )

newtype Email = Email { unEmail :: Text }
  deriving newtype ( IsString, PersistField, ToJSON, FromJSON )

newtype APIToken = APIToken { unAPIToken :: UUID }
  deriving newtype ( ToJSON, FromJSON, FromHttpApiData )

newtype SubmissionUUID = SubmissionUUID { unSubmissionUUID :: UUID }
  deriving newtype ( Show, ToJSON, FromHttpApiData )

newtype MessageHash = MessageHash { unMessageHash :: MD5Digest }
  deriving newtype ( Eq, Binary )

newtype Subtaskname = Subtaskname { unSubtaskname :: ByteString }
  deriving newtype ( Show, Eq, Ord, IsString, PersistField, SqlString, ToLogStr )

instance ToJSON Subtaskname where
  toJSON = String . decodeUtf8 . unSubtaskname

newtype Problemname = Problemname { unProblemname :: ByteString }
  deriving newtype ( Show, Eq, Ord, IsString, PersistField, SqlString, ToLogStr )

instance ToJSON Problemname where
  toJSON = String . decodeUtf8 . unProblemname

newtype Rank = Rank { unRank :: Int }
  deriving newtype ( Show, Eq, Ord, ToJSON, PersistField, PersistFieldSql, ToLogStr )

newtype Score = Score { unScore :: Word64 }
  deriving newtype ( Show, Eq, Ord, ToJSON, PersistField, PersistFieldSql, ToLogStr )

-- | Construct a score from any integral number, this takes min(2^64-1,score)
--   to make it well representable in the DB. Should not matter too much, since such
--   high scores should end up at the bottom of the scoreboard anyway
scoreFromIntegral :: Integral n => n -> Score
scoreFromIntegral s = Score (fromIntegral sc) where
  si = toInteger s
  maxScore = toInteger (maxBound :: Word64)
  sc = if si < 0 then error "Negative score!" else si `min` maxScore

-- * Persist instances to do parsing/unparsing to database values

instance PersistField APIToken where
  toPersistValue = PersistByteString . toStrict . encode . unAPIToken
  fromPersistValue v = fromPersistValue v <&> APIToken . decode . fromStrict

instance PersistFieldSql APIToken where
  sqlType _ = SqlOther "BINARY(16)"

instance PersistField SubmissionUUID where
  toPersistValue = PersistByteString . toStrict . encode . unSubmissionUUID
  fromPersistValue v = fromPersistValue v <&> SubmissionUUID . decode . fromStrict

instance PersistFieldSql SubmissionUUID where
  sqlType _ = SqlOther "BINARY(16)"

instance PersistField MessageHash where
  toPersistValue = PersistByteString . toStrict . encode . unMessageHash
  fromPersistValue v = fromPersistValue v <&> MessageHash . decode . fromStrict

instance PersistFieldSql MessageHash where
  sqlType _ = SqlOther "BINARY(16)"

instance PersistFieldSql Teamname where
  sqlType _ = SqlOther "VARCHAR(128) CHARACTER SET utf8mb4"

instance PersistFieldSql Email where
  sqlType _ = SqlOther "VARCHAR(128) CHARACTER SET utf8mb4"

instance PersistFieldSql Subtaskname where
  sqlType _ = SqlOther "VARCHAR(20) CHARACTER SET ASCII"

instance PersistFieldSql Problemname where
  sqlType _ = SqlOther "VARCHAR(20) CHARACTER SET ASCII"


-- * The actual table definitions

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- All teams that have registered. The API token is a random unique token that allows
-- authentication from that team as well.
Team
  name Teamname
  email Email
  passwordHash (PasswordHash Bcrypt)
  apiToken APIToken

  UniqueEmail email
  UniqueApiToken apiToken

-- A 'submission' from a a team, e.g. every message that they send, and for some of these
-- there is a entry in 'Scores' when they actually scored points with it. The actual message
-- is stored on Object Storage, in the DB we store only the MD5 hash.
Submission
  createdAt UTCTime
  uuid SubmissionUUID
  teamId TeamId
  hash MessageHash
  resultHash MessageHash Maybe

  UniqueUuid uuid

-- List of existing subtasks, for easy reference from other tables
Subtask
  name Subtaskname -- e.g. spaceship

  UniqueSubtask name

SubtaskUnlocked
  teamId TeamId
  subtaskId SubtaskId

  UniqueSubtaskUnlocked teamId subtaskId

-- List of problems, where each problem is part of a subtask
Problem
  name Problemname -- e.g. spaceship1
  subtaskId SubtaskId
  
  UniqueProblemName name

-- Scores for the individual submissions
ScoreRow sql=score
  submissionId SubmissionId
  problemId ProblemId
  score Score

-- Best scores per team per problem
-- this is kept up-to-date by a trigger
ScoreBest
  teamId TeamId
  problemId ProblemId
  score Score

  UniqueScoreBestTeamProblem teamId problemId

-- Scoreboard per subtask
ScoreboardSubtask
  subtaskId SubtaskId
  teamId TeamId
  rank Rank

  UniqueScoreSubtask subtaskId teamId

-- Global scoreboard
ScoreboardGlobal
  teamId TeamId
  rank Rank

  UniqueScoreTeam teamId

CodeSubmission
  teamId TeamId
  submittedAt UTCTime
  url Text
  languages Text
  juryPrize Text
|]

-- * Some other instances

instance Binary SubmissionId where
  put = put . fromSqlKey
  get = toSqlKey <$> B.get

instance Binary TeamId where
  put = put . fromSqlKey
  get = toSqlKey <$> B.get

instance ToLogStr TeamId where
  toLogStr = toLogStr . fromSqlKey

instance ToLogStr SubmissionId where
  toLogStr = toLogStr . fromSqlKey

instance ToJSON MessageHash where
  toJSON = toJSON . show . unMessageHash
