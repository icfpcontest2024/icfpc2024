module ICFPC2024.Puzzles where

import ICFPC2024.Database ( SubmissionId, TeamId, scoreFromIntegral )
import ICFPC2024.Environment ( ServerM, logM, LogPath (..), toLogStr )
import ICFPC2024.AST ( Term(..) )
import qualified ICFPC2024.Puzzles.Hello as HELLO
import qualified ICFPC2024.Puzzles.Efficiency as EFF
import qualified ICFPC2024.Puzzles.Lambdaman as LAM
import qualified ICFPC2024.Puzzles.Spaceship as SPA
import qualified ICFPC2024.Puzzles.ThreeD as THD

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8

-- | Handle the result of the ICFP evaluation, which should be some puzzle command
handleICFPRequest :: SubmissionId -> TeamId -> (Int, ByteString) -> ServerM 'True Term
handleICFPRequest sid tid (len, bs) =
  HELLO.handleRequest sid tid bs
  ?>
  EFF.handleRequest sid tid bs
  ?>
  LAM.handleRequest sid tid (scoreFromIntegral len, bs)
  ?>
  SPA.handleRequest sid tid bs
  ?>
  THD.handleRequest sid tid bs
  ?>
  ( do
      logM Puzzles $ "Submission " <> toLogStr sid <> " got unknown instruction: " <> toLogStr (show $ truncateBS bs)
      -- show to make newlines and such visible
      pure $ TString $ "Unknown instruction: " <> BS8.pack (show $ truncateBS bs)
  )

-- | Helper to
truncateBS :: ByteString -> ByteString
truncateBS bs
  | BS8.length bs <= 20 = bs
  | otherwise = BS8.take 14 bs <> "..." <> BS8.takeEnd 3 bs

-- | Small helper for nice monadic concatenation. Run the first operation, when it
--   returns a 'Just' we return that value, otherwise run the right operation.
(?>) :: Monad m => m (Maybe b) -> m b -> m b
(?>) l r = l >>= \case
  Just t -> pure t
  Nothing -> r

infixr 6 ?>
