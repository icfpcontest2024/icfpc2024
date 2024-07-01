module Main where

import Data.String ( fromString )
import Data.ByteString.Builder ( toLazyByteString )
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as Map

import System.Environment ( getArgs )
import System.Random ( randomRIO )

import ICFPC2024.API ( runWebserver )
import ICFPC2024.Database ( SubmissionId, Submission (..), getSubmission, getTeamByEmail, updatePassword )
import ICFPC2024.Environment ( runServerM, runServerMBG, ServerM, getStaticData )
import ICFPC2024.Communication ( ppParseError, ppInterpreterError )
import ICFPC2024.Scoring ( recomputeScores )
import ICFPC2024.ObjectStorage ( readMessage, Storage (..) )
import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Parser ( pTerm )
import ICFPC2024.Interpreter ( interpret )
import ICFPC2024.Communication ( communicateDistributed )
import ICFPC2024.Puzzles.Static ( static3D )
import ICFPC2024.Puzzles.ThreeD as ThreeD
import ICFPC2024.Puzzles.ThreeD.Static as ThreeD

import Control.Monad ( replicateM )
import Control.Monad.Trans ( lift )
import Database.Persist.Sql ( toSqlKey )

#ifdef PRODUCTION
import ICFPC2024.Communication
  ( runWorker, runWorkerTimeout, getWorkerCount, getQueueLength
  , getTimeoutWorkerCount, getTimeoutQueueLength
  )
#endif

-- ttop git hash: fc2462f265ed92c4ad09b784a4274c5dc3e100e4

main :: IO ()
main = getArgs >>= \case
  [fp] -> runServerM fp runWebserver
  [fp,"--password-reset",email] -> runServerMBG fp $ passwordReset email
  [fp,"--recompute-scoreboard"] -> runServerMBG fp recomputeScores
  [fp,"--download-request",sid] -> runServerMBG fp $ downloadRequest $ toSqlKey $ read sid
  [fp,"--show-request",sid] -> runServerMBG fp $ showRequest $ toSqlKey $ read sid
  [fp,"--resubmit",sid] -> runServerM fp $ resubmit $ toSqlKey $ read sid
  [fp,"--solve-stdin"] -> runServerM fp solveStdin
#ifdef PRODUCTION
  [fp,"--worker",wid] -> runServerM fp $ runWorker $ fromString wid
  [fp,"--worker-timeout",wid] -> runServerM fp $ runWorkerTimeout $ fromString wid
  [fp,"--count-workers"] -> runServerMBG fp $ getWorkerCount >>= lift . print
  [fp,"--queue-length"] -> runServerMBG fp $ getQueueLength >>= lift . print
  [fp,"--timeout-queue-length"] -> runServerMBG fp $ getTimeoutQueueLength >>= lift . print
  [fp,"--timeout-count-workers"] -> runServerMBG fp $ getTimeoutWorkerCount >>= lift . print
#endif
  _ -> putStrLn "Usage: icfpc2024-server config.yaml [--worker wid | --password-reset email | --recompute-scoreboard | --download-request sid | --show-request sid | --resubmit sid | --solve-stdin | --count-workers | --queue-length | --timeout-queue-length | --timeout-count-workers]"


getRequest :: SubmissionId -> (BSL8.ByteString -> ServerM dt ()) -> ServerM dt ()
getRequest sid cont = getSubmission sid >>= \case
  Nothing -> lift $ putStrLn "Submission not found"
  Just s -> readMessage StorageRequest (submissionHash s) >>= cont

-- | Get the raw request data
downloadRequest :: SubmissionId -> ServerM dt ()
downloadRequest sid = getRequest sid $ lift . BSL8.putStr

-- | Pretty print the request & the evaluation result
showRequest :: SubmissionId -> ServerM dt ()
showRequest sid = getRequest sid $ \bs -> parseAndEval bs $ lift . BSL8.putStrLn

parseAndEval :: BSL8.ByteString -> (BSL8.ByteString -> ServerM dt ()) -> ServerM dt ()
parseAndEval bs cont = case pTerm bs of
  Left e -> lift $ BSL8.putStrLn $ toLazyByteString $ "Parse error: " <> ppParseError e
  Right t -> do
    lift $ print t
    case interpret True t of
      Left e -> lift $ BSL8.putStrLn $ toLazyByteString $  "Evaluation error: " <> ppInterpreterError e
      Right (TString v, _) -> cont v
      Right _ -> lift $ BSL8.putStrLn "Expression did not evaluate to a string"

-- | Resubmit a submission on behalf of the team, which can be used in case judging went wrong
resubmit :: SubmissionId -> ServerM 'True ()
resubmit sid = getSubmission sid >>= \case
  Nothing -> lift $ putStrLn "Submission not found"
  Just s -> do
    msg <- readMessage StorageRequest (submissionHash s)
    response <- communicateDistributed (submissionTeamId s) msg
    parseAndEval response $ lift . BSL8.putStrLn

-- | Reset the password of the team with a given email to a random one
passwordReset :: String -> ServerM dt ()
passwordReset em = getTeamByEmail (fromString em) >>= \case
  Nothing -> lift $ putStrLn "Team not found"
  Just tid -> do
    pw <- replicateM 16 $ randomRIO ('~', '!')
    updatePassword tid (fromString pw)
    lift $ putStrLn $ "The new password = " <> pw


solveStdin :: ServerM 'True ()
solveStdin = do
  inp <- lift BSL8.getContents
  parseAndEval inp $ \s -> case BSL8.lines s of
    (firstline:ts)
      | "solve 3d" `BSL8.isPrefixOf` firstline
      , ["solve",nm] <- BSL8.words firstline
      -> do
          ThreeD.StaticData _ _ _ mp <- static3D . fst <$> getStaticData
          lift $ case nm `Map.lookup` mp of
            Nothing -> BSL8.putStrLn $ nm <> " not found"
            Just prob -> case ThreeD.validateSolution nm prob ts of
              Left e -> BSL8.putStrLn $ "Solution for " <> nm <> " is wrong: " <> BSL8.fromStrict e
              Right score -> BSL8.putStrLn $ "Correct, score = " <> BSL8.pack (show score)
    _ -> error "Unknown command"
