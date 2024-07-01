module ICFPC2024.API (
  runWebserver,
) where

import ICFPC2024.Config ( Config (..), TimeConfig(..) )
import ICFPC2024.Environment ( ServerM, getConfig, getStaticData, logM, LogPath (..), toLogStr )
import ICFPC2024.Database
import ICFPC2024.Communication ( communicateDistributed )
import ICFPC2024.Scoring ( getScoreboard )
import ICFPC2024.API.Types
import ICFPC2024.ObjectStorage ( readMessage, Storage (..) )

import Network.Wai
import Network.Wai.Middleware.HttpAuth ( extractBearerAuth )
import Network.Wai.Middleware.RequestSizeLimit
import Network.Wai.Middleware.AddHeaders ( addHeaders )
import Network.Wai.Handler.Warp ( setPort, setServerName, runSettings, defaultSettings )
import Network.HTTP.Types

import Web.HttpApiData ( parseHeader, parseUrlPiece )

import Control.Monad ( forM_, (>=>), join )
import Control.Monad.Trans ( lift )
import Control.Monad.IO.Unlift ( withRunInIO )
import Control.Monad.Catch ( catchAll, throwM )

import Data.Time ( getCurrentTime )
import Data.Binary.Builder ( putStringUtf8 )
import Data.Aeson ( FromJSON, ToJSON, encode, eitherDecode )
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8, encodeUtf8Builder )
import qualified Data.ByteString.Char8 as BS8

import Text.Read ( readMaybe )

#ifdef PRODUCTION
import ICFPC2024.API.RateLimit ( checkRateLimit )
#endif

-- * Main webserver machinery

-- | Run the webserver on the configured port, this is the main entrypoint of the server
runWebserver :: ServerM 'True ()
runWebserver = do
  conf <- getConfig
  -- Make a nice server name
  let settings = setPort (httpPort conf) $ setServerName "UMIX/20.24" defaultSettings
  -- Settings for max body size, we hardcode a global size, and make a nice error message
  let rslSett =
        setMaxLengthForRequest (const $ pure $ Just $ maxBodySize conf) $
        setOnLengthExceeded (\_ _ _ resp -> resp $ responseBuilder requestEntityTooLarge413 [] $
                              "Max body limit of " <> putStringUtf8 (show (maxBodySize conf)) <> " bytes exceeded")
        defaultRequestSizeLimitSettings
  -- Let user know that we're listening
  lift $ putStrLn $ "Listening on port " <> show (httpPort conf)
  -- Then run the server, some unlift magic to run in ServerM 'True instead of IO
  withRunInIO (\unlift -> runSettings settings $
                requestSizeLimitMiddleware rslSett $
                addHeaders
                [ ("Access-Control-Allow-Origin", "*")
                , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                , ("Access-Control-Allow-Headers", "Authorization")
                ] $
                \req resp -> unlift (handler req) >>= resp)

-- | Handler for an individual HTTP request, which should result in a HTTP response
handler :: Request -> ServerM 'True Response
handler req = case pathInfo req of
  ["task"]        -> checkMethod req "GET"  $ checkContestStart taskDescription
  ["language"]    -> checkMethod req "GET"  $ checkContestStart languageDescription
  ["register"]    -> checkMethod req "POST" $ register req
  ["login"]       -> checkMethod req "POST" $ loginH req
  ["team"]        -> checkMethod req "GET"  $ teamInfo req
  ["team","update"] -> checkMethod req "POST"  $ teamUpdate req
  ["team","refresh_token"] -> checkMethod req "POST"  $ teamRefreshToken req
  ["team","history"] -> checkMethod req "GET"  $ communicationHistory req
  ["team","history",gid,"request"] -> checkMethod req "GET"  $ communicationHistoryRequest req gid
  ["team","history",gid,"response"] -> checkMethod req "GET"  $ communicationHistoryResponse req gid
  ["communicate"] -> checkMethod req "POST" $ communicate req
  ["scoreboard"]  -> checkMethod req "GET"  $ getScoreboardGlobal req
  ["scoreboard",nm] -> checkMethod req "GET"  $ getScoreboardSubtask req nm
  ["codesubmit"]  -> checkMethod req "POST" $ codeSubmit req
  _ -> pure $ responseBuilder status404 [] ""


-- * The API handlers

-- | HTML of the task description
taskDescription :: ServerM 'True Response
taskDescription = do
  (_, (task, _)) <- getStaticData
  pure $ responseBuilder status200 [("Content-Type", "text/html")] task

-- | HTML of the language description
languageDescription :: ServerM 'True Response
languageDescription = do
  (_, (_, lang)) <- getStaticData
  pure $ responseBuilder status200 [("Content-Type", "text/html")] lang

-- | Register a new team
register :: Request -> ServerM dt Response
register req = withJSONBody req $ \tm -> do
  registerTeam (name tm) (email tm) (password tm) >>= \case
    Left e -> pure $ responseBuilder status400 [] e
    Right tk -> returnJSON tk

-- | Use username and password to obtain an APIToken
loginH :: Request -> ServerM dt Response
loginH req = withJSONBody req $ \tm -> do
  login (loginEmail tm) (loginPassword tm) >>= \case
    Just tk -> returnJSON tk
    Nothing -> pure $ responseBuilder status401 [] ""

-- | Get team info based on the token
teamInfo :: Request -> ServerM dt Response
teamInfo req = withAuthentication req $ \tid -> do
  (nm,em) <- getTeam tid
  returnJSON $ TeamInfo { tiName = nm, tiEmail = em }

-- | Update team details, based on API token + password auth
teamUpdate :: Request -> ServerM dt Response
teamUpdate req = withAuthentication req $ \tid -> withJSONBody req $ \upd -> do
  -- Now login with the old email to check the given apssword
  (_,em) <- getTeam tid
  login em (updPassword upd) >>= \case
    Nothing -> pure $ responseBuilder status401 [] ""
    Just _ -> do
      updateTeam tid (updName upd) (updEmail upd)
      forM_ (updNewPassword upd) $ updatePassword tid
      pure $ responseBuilder status204 [] ""

-- | Generate a new API token for a team
teamRefreshToken :: Request -> ServerM dt Response
teamRefreshToken req = withAPIToken req $ refreshAPIToken >=> \case
  Nothing -> pure $ responseBuilder status401 [] "Authentication failed"
  Just tk -> returnJSON tk

communicationHistory :: Request -> ServerM dt Response
communicationHistory req = withAuthentication req $ \tid -> do
  -- Check if we need a different page
  let page = readMaybe . BS8.unpack =<< join ("page" `lookup` queryString req)
  -- Get the right page and serve it
  submissions <- getSubmitHistory tid page
  returnJSON $ flip map submissions $ \sub ->
    CommunicateHistory
    { chCreatedAt = submissionCreatedAt sub
    , chUuid      = submissionUuid sub
    , chRequest   = submissionHash sub
    , chResponse  = submissionResultHash sub
    }

-- | Download an individual request from the history
communicationHistoryRequest :: Request -> Text -> ServerM dt Response
communicationHistoryRequest = communicationHistoryHelper' StorageRequest (Just . submissionHash)

-- | Download an individual response from the history
communicationHistoryResponse :: Request -> Text -> ServerM dt Response
communicationHistoryResponse = communicationHistoryHelper' StorageResponse submissionResultHash

-- | Generalization of 'communicationHistoryRequest' and 'communicationHistoryResponse'
communicationHistoryHelper' :: Storage -> (Submission -> Maybe MessageHash) -> Request -> Text -> ServerM dt Response
communicationHistoryHelper' storage getMbHash req suuid = withAuthentication req $ \tid ->
  case parseUrlPiece suuid of
    Left e -> pure $ responseLBS status400 [] $ BS8.fromStrict $ encodeUtf8 e
    Right uuid -> getSubmissionByUuid tid uuid >>= \case
      Nothing -> pure $ responseBuilder status404 [] ""
      Just sub -> case getMbHash sub of
        Nothing -> pure $ responseBuilder status404 [] ""
        Just hash -> do
          bs <- readMessage storage hash
          pure $ responseLBS status200 [(hContentType, "text/icfp")] bs


-- | Communicate with the Cult of the Bound Variable
communicate :: Request -> ServerM 'True Response
communicate req = withAuthentication req $ \tid -> checkRateLimit tid $ checkContestTimes $ do
  body <- lift (lazyRequestBody req)
  bs <- communicateDistributed tid body
    `catchAll`
    (\e -> do
        logM Errors $ "Request for team " <> toLogStr tid <> " gave an exception: " <> toLogStr (show e)
        throwM e
    )
  pure $ responseLBS status200 [(hContentType, "text/icfp")] bs

-- | Final code submission
codeSubmit :: Request -> ServerM dt Response
codeSubmit req = withAuthentication req $ \tid -> withJSONBody req $ \s -> do
  saveCodeSubmission tid (url s) (language s) (juryprize s)
  pure $ responseBuilder status204 [] ""

-- | Get the global scoreboard
getScoreboardGlobal :: Request -> ServerM dt Response
getScoreboardGlobal req = withAuthenticationOpt req $ \mbTid ->
  getScoreboard mbTid Nothing >>= returnJSON

-- | Get the scoreboard for a given subtask
getScoreboardSubtask :: Request ->Text -> ServerM dt Response
getScoreboardSubtask req tnm = withAuthenticationOpt req $ \mbTid ->
  getSubtaskId (Subtaskname $ encodeUtf8 tnm) >>= \case
    Nothing -> pure $ responseBuilder status404 [] ""
    Just stid -> getScoreboard mbTid (Just stid) >>= returnJSON

-- * Helpers

-- | Helper to verify that the request method is as expected
checkMethod :: Request -> Method -> ServerM dt Response -> ServerM dt Response
checkMethod req meth cont
  | requestMethod req == meth = cont
  -- For CORS, we respond with 200 to an options request
  | requestMethod req == "OPTIONS" = pure $ responseBuilder status200 [] ""
  | otherwise = pure $ responseBuilder status405 [] ""

-- | Helper to parse the JSON body and send errors back to the user
withJSONBody :: FromJSON a => Request -> (a -> ServerM dt Response) -> ServerM dt Response
withJSONBody req cont = lift (lazyRequestBody req) >>= \bs -> case eitherDecode bs of
  Left e -> pure $ responseBuilder status400 [] (putStringUtf8 e)
  Right tm -> cont tm

-- | Return a JSON-encoded object to the client
returnJSON :: ToJSON a => a -> ServerM dt Response
returnJSON r = pure $ responseLBS status200 [(hContentType, "application/json")] $ encode r

-- | Continue only with a valid Bearer token
withAPIToken :: Request -> (APIToken -> ServerM dt Response) -> ServerM dt Response
withAPIToken req cont = case (hAuthorization `lookup` requestHeaders req) >>= extractBearerAuth of
  Nothing -> pure $ responseBuilder status401 [] "Missing bearer token"
  Just bs -> case parseHeader bs of
    Left e -> pure $ responseBuilder status401 [] (encodeUtf8Builder e)
    Right tk -> cont tk

-- | Authenticate a team based on the Bearer token
withAuthentication :: Request -> (TeamId -> ServerM dt Response) -> ServerM dt Response
withAuthentication req cont = withAPIToken req $ authenticate >=> \case
  Nothing -> pure $ responseBuilder status401 [] "Authentication failed"
  Just tid -> cont tid

-- | Authenticate a team based on the Bearer token, but authentication is full optional
withAuthenticationOpt :: Request -> (Maybe TeamId -> ServerM dt Response) -> ServerM dt Response
withAuthenticationOpt req cont = case (hAuthorization `lookup` requestHeaders req) >>= extractBearerAuth of
  Nothing -> cont Nothing
  Just bs -> case parseHeader bs of
    Left _ -> cont Nothing
    Right tk -> authenticate tk >>= \case
      Nothing -> cont Nothing
      Just tid -> cont $ Just tid

-- | Run the continuation only when the contest has started
checkContestStart :: ServerM dt Response -> ServerM dt Response
checkContestStart cont = do
  tims <- contestTimes <$> getConfig
  now <- lift getCurrentTime
  if now < contestStart tims
    then pure $ responseLBS status403 [(hContentType, "application/json")] $ "{\"contestStart\":" <> encode (contestStart tims) <> "}"
    else cont

-- | Run the continuation only when the contest did not end yet
checkContestEnd :: ServerM dt Response -> ServerM dt Response
checkContestEnd cont = do
  tims <- contestTimes <$> getConfig
  now <- lift getCurrentTime
  if contestEnd tims < now
    then pure $ responseLBS status403 [(hContentType, "application/json")] $ "{\"contestEnd\":" <> encode (contestEnd tims) <> "}"
    else cont

-- | Run the continuation only when the contest is currently running
checkContestTimes :: ServerM dt Response -> ServerM dt Response
checkContestTimes = checkContestStart . checkContestEnd

#ifndef PRODUCTION
checkRateLimit :: TeamId -> ServerM dt Response -> ServerM dt Response
checkRateLimit _ = id
#endif
