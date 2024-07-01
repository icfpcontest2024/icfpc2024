module TTop (
  Volume,
  Outcome (..),
  runProgram,
  renderOutcome,
) where

import TTop.Board
import TTop.History as History
import Control.Monad.Trans.Except
import Data.Maybe ( fromMaybe )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS8

type Volume = Integer

data Outcome
  = Terminated Volume
  | Submitted Value Volume
  | Crashed RuntimeError

runProgram :: Bool -> Maybe Int -> State -> ([State], Outcome)
runProgram disableOpt maxTicks st = case runExcept $ History.tick disableOpt (fromMaybe 1_000_000 maxTicks) st of
  Left NoChange -> ([st], Terminated (volume st))
  Left err -> ([st], Crashed err)
  Right (Right val) -> ([st], Submitted val (volume st))
  Right (Left st') -> let ~(ss, out) = runProgram disableOpt maxTicks st' in (st : ss, out)

renderOutcome :: Outcome -> [ByteString]
renderOutcome (Terminated vol) =
  [ "No answer submitted."
  , "Volume spent: " <> BS8.pack (show vol)
  ]
renderOutcome (Submitted val vol) =
  [ "Submitted answer: " <> render (pretty val)
  , "Volume spent: " <> BS8.pack (show vol)
  ]
renderOutcome (Crashed err) =
  [ "Crashed: " <> BS8.pack (show err)
  ]
