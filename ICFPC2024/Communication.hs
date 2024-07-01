{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ICFPC2024.Communication (
  communicateDistributed,
  ppParseError,
  ppInterpreterError,
#ifdef PRODUCTION
  runWorker,
  runWorkerTimeout,
  getWorkerCount,
  getQueueLength,
  getTimeoutWorkerCount,
  getTimeoutQueueLength,
#endif
) where

import Data.Set ( Set )
import Data.ByteString.Builder ( Builder, char7, int64Dec, toLazyByteString )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Set as Set

import ICFPC2024.AST ( Term (TString), charsDecoded )
import ICFPC2024.Config ( Config (..) )
import ICFPC2024.Environment ( ServerM, getConfig, logM, LogPath (..), toLogStr )
import ICFPC2024.Interpreter ( interpret, InterpreterError (..) )
import ICFPC2024.ObjectStorage ( storeMessage, readMessage, Storage (..) )
import ICFPC2024.Parser ( pTerm, ParseError (..) )
import ICFPC2024.Printer ( ppTerm )
import ICFPC2024.Puzzles ( handleICFPRequest )
import ICFPC2024.Database ( TeamId, SubmissionId, MessageHash (..), storeSubmission, storeSubmissionResult )

import System.Timeout ( timeout )

import Control.Monad.Catch ( catch )
import Control.Monad.IO.Unlift ( withRunInIO )
import GHC.IO.Exception ( AsyncException )

#ifdef PRODUCTION
import Data.Maybe ( listToMaybe )
import Data.UUID ( UUID )
import Data.Functor ( (<&>) )
import Data.Binary ( encode )
import Data.Proxy ( Proxy (..) )
import Data.Digest.Pure.MD5 ( md5 )

import Database.Redis.Schema
import Database.Redis.Schema.RemoteJob

import ICFPC2024.Environment ( getRedisPool )

import System.Signal ( installHandler, sigINT, sigTERM )

import Control.Monad ( void )
import Control.Monad.Trans ( lift )
import Control.Concurrent.MVar ( newEmptyMVar, tryPutMVar, takeMVar )
#endif

-- * Main communication

-- | Run a communication 'job', where the input is the ICFP expression from the contestants,
--   and the response is the response ICFP expression.
--   This is NOT a pure function, it has the side-effect of writing to the database,
--   so we don't cache and we re-evaluate everything.
communicateDistributed :: TeamId -> ByteString -> ServerM 'True ByteString
communicateDistributed tid bs = do
  hash <- storeMessage StorageRequest bs
  sid <- storeSubmission tid hash
#ifdef PRODUCTION
  local <- localWorker <$> getConfig
  tow <- timeoutWorker <$> getConfig
  rhash <- case local of
    True -> handleJob (sid, tid, hash)
    False -> getRedisPool >>= \pool -> runRemoteJob @ICFPCQueue True pool 1 (sid, tid, hash) >>= \case
      Left e  -> error $ "Remote job error: " <> show e
      -- Hacky special case, it got a time or memory error, so we try again in a dedicated queue...
      Right r | tow && r `elem` retryHashes -> runRemoteJob @ICFPCQueueTimeout True pool 1 (sid, tid, hash) >>= \case
        Left e  -> error $ "Remote job error for timeout worker: " <> show e
        Right r2 -> pure r2
      Right r -> pure r
#else
  rhash <- handleJob (sid, tid, hash)
#endif
  storeSubmissionResult sid rhash
  readMessage StorageResponse rhash

-- * Worker

-- | The main worker handler, which works based on md5 hashes
handleJob :: (SubmissionId, TeamId, MessageHash) -> ServerM 'True MessageHash
handleJob (sid, tid, reqHash) = do
  bs <- readMessage StorageRequest reqHash
  maxTimeUsec <- (1_000_000 *) . maxRuntimeSec <$> getConfig
  mbResp <- withRunInIO $ \unlift -> do
    timeout maxTimeUsec $ unlift $ handleJob' sid tid bs
      `catch` (\(e :: AsyncException) -> do
                  logM Errors $ "Exception in handleJob for submission " <> toLogStr sid <> ": " <> toLogStr (show e)
                  pure memLimitError
              )
  resp <- case mbResp of
    Just r -> pure r
    Nothing -> do
      logM Errors $ "Time limit reached for submission " <> toLogStr sid
      pure timeLimitError
  storeMessage StorageResponse resp

-- | Handle a communication job locally based on the actual ByteStrings
handleJob' :: SubmissionId -> TeamId -> ByteString -> ServerM 'True ByteString
handleJob' sid tid bs = ppTerm <$> case pTerm bs of
  Left e -> pure $ TString $ toLazyByteString $ "Parse error: " <> ppParseError e
  Right t -> case interpret True t of
    Left e -> pure $ TString $ toLazyByteString $ "Evaluation error: " <> ppInterpreterError e
    Right (TString v, _) -> handleICFPRequest sid tid (fromIntegral $ BS8.length bs, v)
    Right _ -> pure $ TString "Expression did not evaluate to a string"

#ifdef PRODUCTION

-- | The queue that contains all incoming messages to be processed
data ICFPCQueue

instance JobQueue ICFPCQueue where
  type RPC ICFPCQueue =
    '[ (SubmissionId, TeamId, MessageHash) -> MessageHash
     ]
  keyPrefix = "icfpc2024:q"

-- | The queue for messages getting a timeout first
data ICFPCQueueTimeout

instance JobQueue ICFPCQueueTimeout where
  type RPC ICFPCQueueTimeout =
    '[ (SubmissionId, TeamId, MessageHash) -> MessageHash
     ]
  keyPrefix = "icfpc2024:tq"


-- | Main entrypoint for workers, this will pick up jobs from the job queue.
--   Important: WorkerId must be unique!
runWorker ::  WorkerId -> ServerM 'True ()
runWorker workerId = do
  -- First check if there is a running job for this worker, if so then we
  -- crashed so we send back a nice message
  getRunningJob (Proxy @ICFPCQueue) workerId >>= \case
    Nothing -> pure ()
    Just job -> do
      logM Errors $ "Found a running job for worker " <> toLogStr (show workerId)
      hash <- storeMessage StorageResponse crashResponse
      sendJobResponseInternal (Proxy @ICFPCQueue) job hash

  -- Then we start the actual worker
  let err e = logM Errors $ "Error in worker: " <> toLogStr (show e)
  pool <- getRedisPool
  lift $ putStrLn $ "Starting worker " <> show workerId

  -- Install Ctrl+C and sigterm handler
  ctrlC <- lift newEmptyMVar
  lift $ installHandler sigINT  (const $ void $ tryPutMVar ctrlC ())
  lift $ installHandler sigTERM (const $ void $ tryPutMVar ctrlC ())

  -- Run the job worker in a separate thread
  withRunInIO $ \unlift ->
    withRemoteJobWorker @ICFPCQueue workerId pool (unlift . err)
    (\workerHandle -> do
        -- When ctrlC is pressed, we start the graceful shutdown
        takeMVar ctrlC
        putStrLn $ "Gracefully shutting down worker " <> show workerId
        gracefulShutdown workerHandle
    ) (unlift . handleJob)


-- | The worker for the timeout queue, so much copy & paste, but generalizing over the queue type was hard....
runWorkerTimeout ::  WorkerId -> ServerM 'True ()
runWorkerTimeout workerId = do
  -- First check if there is a running job for this worker, if so then we
  -- crashed so we send back a nice message
  getRunningJob (Proxy @ICFPCQueueTimeout) workerId >>= \case
    Nothing -> pure ()
    Just job -> do
      logM Errors $ "Found a running job for worker " <> toLogStr (show workerId)
      hash <- storeMessage StorageResponse crashResponse
      sendJobResponseInternal (Proxy @ICFPCQueueTimeout) job hash

  -- Then we start the actual worker
  let err e = logM Errors $ "Error in worker: " <> toLogStr (show e)
  pool <- getRedisPool
  lift $ putStrLn $ "Starting worker " <> show workerId

  -- Install Ctrl+C and sigterm handler
  ctrlC <- lift newEmptyMVar
  lift $ installHandler sigINT  (const $ void $ tryPutMVar ctrlC ())
  lift $ installHandler sigTERM (const $ void $ tryPutMVar ctrlC ())

  -- Run the job worker in a separate thread
  withRunInIO $ \unlift ->
    withRemoteJobWorker @ICFPCQueueTimeout workerId pool (unlift . err)
    (\workerHandle -> do
        -- When ctrlC is pressed, we start the graceful shutdown
        takeMVar ctrlC
        putStrLn $ "Gracefully shutting down worker " <> show workerId
        gracefulShutdown workerHandle
    ) (unlift . handleJob)

-- * Worker helpers, works around 'Database.Redis.Schema.RemoteJob' not exporting
--   everything, so we go the hacky way and make assumptions about it's
--   internals (which is fair since it is also written by me, and I don't want to
--   change the public library for the ICFP contest ;-) )

type RunningJob = (WorkerId, (UUID, Int, ByteString))

data RunningJobs q = RunningJobs
instance JobQueue q => Ref (RunningJobs q) where
  type ValueType (RunningJobs q) = Set RunningJob
  toIdentifier RunningJobs = colonSep [keyPrefix @q, "running"]

-- | Get the currently running task for a worker. This is used in case
--   the worker crashed, to send back a nice reply after the restart
getRunningJob :: forall q. JobQueue q => Proxy q -> WorkerId -> ServerM 'True (Maybe RunningJob)
getRunningJob _ wid = do
  pool <- getRedisPool
  run pool (get $ RunningJobs @q) <&> \case
    Nothing -> Nothing
    Just jobs -> listToMaybe
      [ job
      | job@(worker, _) <- Set.toList jobs
      , worker == wid
      ]

newtype ResultBox q = ResultBox UUID
instance JobQueue q => Ref (ResultBox q) where
  type ValueType (ResultBox q) = [Either RemoteJobError ByteString]
  toIdentifier (ResultBox uuid) =
    colonSep [keyPrefix @q, "result", toBS uuid]

-- | RemoteJob internal function to send a response to a job
sendJobResponseInternal :: forall q. JobQueue q => Proxy q -> RunningJob -> MessageHash -> ServerM 'True ()
sendJobResponseInternal _ job@(_, (uuid,_,_)) resp = do
  pool <- getRedisPool
  let box = ResultBox @q uuid
  run pool $ do
    lPushLeft box [Right $ encode resp]
    sDelete (RunningJobs @q) [job]

-- * Server status

getWorkerCount :: ServerM dt Integer
getWorkerCount = do
  pool <- getRedisPool
  run pool $ countWorkers @ICFPCQueue

getQueueLength :: ServerM dt Integer
getQueueLength = do
  pool <- getRedisPool
  run pool $ queueLength @ICFPCQueue

getTimeoutWorkerCount :: ServerM dt Integer
getTimeoutWorkerCount = do
  pool <- getRedisPool
  run pool $ countWorkers @ICFPCQueueTimeout

getTimeoutQueueLength :: ServerM dt Integer
getTimeoutQueueLength = do
  pool <- getRedisPool
  run pool $ queueLength @ICFPCQueueTimeout

#endif

-- * Helpers

char7safe :: Char -> Builder
char7safe c
  | c `elem` validChars = char7 c
  | otherwise = "[UNREPRESENTABLE CHARACTER]"

-- | Pretty print a parser error
ppParseError :: ParseError -> Builder
ppParseError UnexpectedEOF = "Unexpected EOF"
ppParseError (UnusedInput i) = "Unused input at index " <> int64Dec i
ppParseError (UnexpectedChar c i) = "Unexpected character '" <> char7safe c <> "' at index " <> int64Dec i

validChars :: Set Char
validChars = Set.fromList charsDecoded

-- | Pretty print an interpreter error
ppInterpreterError :: InterpreterError -> Builder
ppInterpreterError BetaReductionLimit = "Maximum number of beta reductions exceeded"
ppInterpreterError ScopeError = "Scope error"
ppInterpreterError ArithmeticError = "Arithmetic error"
ppInterpreterError TypeError = "Type error"
ppInterpreterError (UnknownUnOp c) = "Reference to unknown unary op '" <> char7safe c <> "'"
ppInterpreterError (UnknownBinOp c) = "Reference to unknown binary op '" <> char7safe c <> "'"

-- | Error we send back when the max runtime is exceeded
timeLimitError :: ByteString
timeLimitError = ppTerm $ TString "Time limit exceeded"

-- | Error we send back when an async exception happens, which is usually when the stack or heap is full
memLimitError :: ByteString
memLimitError = ppTerm $ TString "Memory limit exceeded"

#if PRODUCTION
-- | Error when the worker crashed
crashResponse :: ByteString
crashResponse = ppTerm $ TString "Something went wrong, this is likely due to an out-of-memory error"

-- | Hashes of response message for which we want to retry on a beefier machine
retryHashes :: [MessageHash]
retryHashes = map (MessageHash . md5) [timeLimitError, memLimitError, crashResponse]
#endif
