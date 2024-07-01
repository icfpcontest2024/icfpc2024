module TTop.History
  ( RuntimeError(..)
  , State
  , newState
  , renderState
  , volume
  , TTop.History.tick
  ) where

import Data.Foldable
import Data.ByteString.Char8 (ByteString)
import Data.Set (Set)
import Control.Monad.Trans.Except
import qualified Data.Set as Set
-- import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import TTop.Board as Board
import qualified TTop.Bimap as Bimap

data Bounds = Bounds
  { minX :: Int, maxX :: Int
  , minY :: Int, maxY :: Int
  , minT :: Int, maxT :: Int
  }

instance Semigroup Bounds where
  Bounds minX maxX minY maxY minT maxT <> Bounds minX' maxX' minY' maxY' minT' maxT'
    = Bounds
        (minX `min` minX') (maxX `max` maxX')
        (minY `min` minY') (maxY `max` maxY')
        (minT `min` minT') (maxT `max` maxT')

data State = State
  { history :: [Board]
  , t :: Int  -- same as (length history)
  , ticks :: Int  -- for time limiting
  , bounds :: Bounds
  }

volume :: State -> Integer
volume st =
  fromIntegral (st.bounds.maxX - st.bounds.minX + 1)
  * fromIntegral (st.bounds.maxY - st.bounds.minY + 1)
  * fromIntegral (st.bounds.maxT - st.bounds.minT + 1)

getBounds :: Int -> Board -> Bounds
getBounds t brd = Bounds minX maxX minY maxY t t
  where
    (minX, maxX, minY, maxY) = Bimap.bounds brd.cells

newState :: Inputs -> Board -> State
newState inputs brd =
  let brd' = fillInputs inputs brd
  in State
     { history = [brd']
     , t = 1
     , ticks = 0
     , bounds = getBounds 1 brd'
     }

fillInputs :: Inputs -> Board -> Board
fillInputs inputs brd = brd{ cells = Bimap.map f brd.cells }
  where
    f (Input i) | fromInteger i < length inputs = inputs !! fromInteger i
    f x = x

applyWarp :: Board -> Warp -> Board
applyWarp brd w = Board
  { cells = Bimap.insert w.x w.y w.value brd.cells
  , dirty = Set.insert (w.x, w.y) brd.dirty
  , opMap = brd.opMap  -- this will be taken care of at the end
  }

nSubmits :: Board -> Warp -> Set Value
nSubmits brd w = case Bimap.lookup w.x w.y brd.cells of
  Just (Op OpSubmit) -> Set.singleton w.value
  _ -> Set.empty

appendBoard :: Board -> State -> State
appendBoard brd st = State
  { history = brd : st.history
  , t = st.t + 1
  , ticks = st.ticks + 1
  , bounds = st.bounds <> getBounds (st.t+1) brd
  }

data RuntimeError
  = ConflictingWarps [Warp]
  | TooManySubmits [Value]
  | WarpToInvalidDt Int
  | WarpToConflictingTimes [Time]
  | WarpDtMustBePositive Int
  | NoChange  -- without submitting a value
  | TickLimitExceeded
  | BoardError BoardError
  deriving stock (Show)

safeDrop :: Int -> [a] -> Maybe [a]
safeDrop 0 bs = Just bs
safeDrop k (_:bs) = safeDrop (k-1) bs
safeDrop _ [] = Nothing

tick :: Bool -> Int -> State -> Except RuntimeError (Either State Value)
tick disableOpt maxTicks st =
  case Board.tick disableOpt brdPrev of
    Left be -> throwE (BoardError be)
    Right (brdUpdated, warpsUpdated, submits)
      | st.ticks >= maxTicks
      -> throwE TickLimitExceeded

      | brdUpdated.cells == brdPrev.cells
      , null warpsUpdated
      -> throwE NoChange

      | conflictingWarps <- findConflictsBy (\w -> [(w.x, w.y)]) warpsUpdated
      , not (null conflictingWarps)
      -> throwE $ ConflictingWarps (Set.toList conflictingWarps)

      | Set.size submits > 1
      -> throwE $ TooManySubmits (Set.toList submits)

      | Set.size submits == 1
      -> pure $ Right (head $ Set.toList submits)

      | otherwise
      -> case unique [w.dt | w <- warpsUpdated] of
          -- no warps
          [] -> pure $ Left (appendBoard brdUpdated st)

          [dt]
            | dt <= 0 -> throwE $ WarpDtMustBePositive dt
            | otherwise -> case safeDrop dt st.history of
                Nothing -> throwE $ WarpToInvalidDt dt
                Just [] -> throwE $ WarpToInvalidDt dt
                Just (bOld:bs) -> case Set.toList $ Set.unions [nSubmits bOld w | w <- warpsUpdated] of
                  -- no warps into submit operators
                  [] -> let bNewRaw = foldl' applyWarp bOld warpsUpdated
                            doesWarpOps = not $ null [() | Warp _ _ _ (Op _) <- warpsUpdated]
                            doesOverwriteOps = not $ null
                              [() | w <- warpsUpdated, Just (Op _) <- [Bimap.lookup w.x w.y bOld.cells]]
                            bNew
                              | doesWarpOps || doesOverwriteOps = bNewRaw
                                { dirty = allCoords bNewRaw.cells
                                , opMap = rebuildOpMap bNewRaw.cells
                                }
                              | otherwise = bNewRaw
                        in pure $ Left State
                          { history = bNew : bs
                          , t = st.t - dt
                          , ticks = st.ticks + 1
                          , bounds = st.bounds <> getBounds (st.t-dt) bNew
                          }

                  -- exactly one warp into a submit operator
                  [submission] -> pure $ Right submission

                  -- conflicting submissions arising from warp
                  submissions -> throwE $ TooManySubmits submissions

          -- conflicting warp times
          dts -> throwE $ WarpToConflictingTimes [st.t-dt | dt <- dts]
  where
    brdPrev = head st.history
    unique = Set.toList . Set.fromList

renderState :: State -> ByteString
renderState st = header <> "\n" <> render (pretty brd)
  where
    brd = head st.history
    bounds = getBounds st.t brd
    header = mconcat
      [ "[t=", BS8.pack (show st.t)
      , ", x=", BS8.pack (show bounds.minX)
      , ", y=", BS8.pack (show bounds.minY)
      {-
      -- for debugging
      , ", dirty=", BS8.pack (show $ Set.toList brd.dirty)
      , ", opMap=", BS8.pack (show $ Map.toList brd.opMap)
      -}
      , "]"
      ]
