module ICFPC2024.Puzzles.Hello (
  handleRequest,
) where

import ICFPC2024.AST ( Term (..), charsDecoded )
import ICFPC2024.Environment ( ServerM, getStaticData )
import ICFPC2024.Database ( SubmissionId, TeamId, SubtaskId, Subtaskname (..), getSubtaskId )
import ICFPC2024.Scoring ( getScoreboard, Scoreboard (..), ScoreboardRow (..), storeScore, getSubtasksUnlocked )
import ICFPC2024.Puzzles.Helpers.Static ( renderTemplate )
import ICFPC2024.Puzzles.Static ( staticHello )
import ICFPC2024.Puzzles.Hello.Static

import Data.Aeson ( Value(String, Null), toJSON )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Map ( Map )
import Data.Maybe ( isJust )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Monad ( void )

-- | Handle an incoming ICFP request, or return Nothing when the request is not part of this subtask
handleRequest :: SubmissionId -> TeamId -> ByteString -> ServerM 'True (Maybe Term)
handleRequest sid tid "get index" = do
  -- Getting the index is hello1
  void $ storeScore sid tid "hello1" Nothing

  -- Get the unlocked subtasks
  unlocked <- getSubtasksUnlocked tid
  sd <- staticHello . fst <$> getStaticData
  let vars :: Map String Value
      vars = Map.fromList
        [ ("subtasks", toJSON unlocked)
        , ("is_enrolled", toJSON $ not $ null unlocked)
        ]
  pure $ Just $ renderTemplate (sdIndex sd) vars
handleRequest sid tid "get scoreboard" = Just <$> renderScoreboard sid tid Nothing
handleRequest sid tid s
  | "get scoreboard " `BSL8.isPrefixOf` s
  , ["get", "scoreboard", stnm] <- BSL8.words s = getSubtaskId (Subtaskname $ BSL8.toStrict stnm) >>= \case
      Nothing -> pure Nothing
      Just stid -> Just <$> renderScoreboard sid tid (Just stid)
handleRequest _ _ "get echo" = do
  sd <- staticHello . fst <$> getStaticData
  pure $ Just $ renderTemplate (sdEcho sd) ()
-- Catch 'cheaters' that try to use the echo service to solve the self-check
handleRequest _ _ "echo Self-check OK, send `solve language_test 4w3s0m3` to claim points for it"
  = pure $ Just $ TString "You should take the language test yourself"
handleRequest sid tid s
  | "echo " `BSL8.isPrefixOf` s = do
      -- The echo service is hello3
      void $ storeScore sid tid "hello3" Nothing
      -- And then serve the response
      sd <- staticHello . fst <$> getStaticData
      let vars :: Map String Text
          vars = Map.singleton "response" (decodeUtf8 $ BSL8.toStrict $ BSL8.drop 5 s)
      pure $ Just $ renderTemplate (sdEchoR sd) vars
handleRequest _ _ "get language_test" = do
  sd <- staticHello . fst <$> getStaticData
  pure $ Just $ sdSelfCheck sd
handleRequest sid tid "solve language_test 4w3s0m3" = do
  -- The self_check is hello4
  Just <$> storeScore sid tid "hello4" Nothing
handleRequest _ _ _ = pure Nothing


-- | Scoreboard rendering in Markdown
renderScoreboard :: SubmissionId -> TeamId -> Maybe SubtaskId -> ServerM 'True Term
renderScoreboard sid tid mbStid = do
  -- Getting the scoreboard is hello2
  void $ storeScore sid tid "hello2" Nothing
  -- Get the scoreboard data
  sc <- getScoreboard (Just tid) mbStid
  sd <- staticHello . fst <$> getStaticData
  -- Make the data ready for mustache, we can't render null values,
  -- and also not all teamnames, so we just replace some characters by question marks
  let validChars = Set.fromList charsDecoded
  let cleanValue (String s) = String $ T.map (\x -> if x `Set.member` validChars then x else '?') s
      cleanValue Null = String ""
      cleanValue v = v
  let sc' = sc { rows = map (\r -> r { values = map cleanValue (values r) }) (rows sc) }
  let vars :: Map String Value
      vars = Map.fromList
        [ ("scoreboard", toJSON sc')
        , ("is_subtask", toJSON $ isJust mbStid)
        ]
  pure $ renderTemplate (sdScoreboard sd) vars
