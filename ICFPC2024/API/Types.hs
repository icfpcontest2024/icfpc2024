module ICFPC2024.API.Types where

import ICFPC2024.Database

import GHC.Generics ( Generic )

import Data.Char ( toLower )
import Data.Aeson
import Data.Aeson.Casing ( aesonDrop, camelCase )
import Data.Text ( Text )
import Data.Time ( UTCTime )

-- | POST body of a team registration
data RegisterTeam = RegisterTeam {
  name     :: Teamname,
  email    :: Email,
  password :: Password
  } deriving ( Generic )
instance FromJSON RegisterTeam

-- | POST body of a login
data Login = Login {
  loginEmail    :: Email,
  loginPassword :: Password
  } deriving ( Generic )

instance FromJSON Login where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 5 }

-- | POST body of a team update
data UpdateTeam = UpdateTeam {
  updName     :: Teamname,
  updEmail    :: Email,
  updPassword :: Password,
  updNewPassword :: Maybe Password
  } deriving ( Generic )

instance FromJSON UpdateTeam where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 3 }

-- | POST body of final code submission
data CodeSubmit = CodeSubmit {
  url       :: Text,
  language  :: Text,
  juryprize :: Text
  } deriving ( Generic )

instance FromJSON CodeSubmit

-- | Response for getting a team
data TeamInfo = TeamInfo {
  tiName   :: Teamname,
  tiEmail  :: Email
  } deriving ( Generic )

instance ToJSON TeamInfo where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 2 }


data CommunicateHistory = CommunicateHistory
  { chCreatedAt :: UTCTime
  , chUuid      :: SubmissionUUID
  , chRequest   :: MessageHash
  , chResponse  :: Maybe MessageHash
  } deriving ( Generic )

instance ToJSON CommunicateHistory where
  toJSON = genericToJSON $ aesonDrop 2 camelCase
