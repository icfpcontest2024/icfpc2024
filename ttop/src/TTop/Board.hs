module TTop.Board
  ( Time
  , Board(..)
  , Warp(..)
  , Inputs
  , Value(..)
  , Operator(..)
  , BoardError(..)
  , tick
  , allCoords
  , rebuildOpMap
  , findConflictsBy
  , render
  , pretty
  , parseBoard
  , formatText
  , formatCsv
  , formatTextNoComments
  ) where

import GHC.Generics
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS
import Data.Set (Set)
import Data.Maybe (catMaybes)
import Data.Char (ord,chr)
import Data.List (transpose, foldl')
import Text.Read (readMaybe)
import Data.ByteString.Char8 ( ByteString )
import TTop.Bimap (Bimap)
import Data.Map.Strict (Map)
import qualified TTop.Bimap as Bimap
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS8

type Time = Int

data Operator
  = OpLeft
  | OpRight
  | OpUp
  | OpDown
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpMod
  | OpWarp
  | OpEq
  | OpNeq
  | OpSubmit
  deriving stock (Show, Eq, Ord, Generic)

data Value = I Integer | Op Operator | Input Integer
  deriving stock (Show, Eq, Ord, Generic)

type Point = (Int, Int)

-- When you write a value into a cell,
-- you need to find out which operators could be activated by this write.
-- OpMap tells you exactly that.
type OpMap = Map Point [(Point, Value)]
type Cells = Bimap Value

data Board = Board
  { cells :: Cells
  , dirty :: Set Point
  , opMap :: OpMap
  }

data Warp = Warp
  { dt :: Int
  , x :: Int
  , y :: Int
  , value :: Value
  }
  deriving stock (Show, Generic, Eq, Ord)

data Edit = Edit
  { takes :: [Point]
  , puts :: [(Point, Value)]
  , warps :: [Warp]
  }
  deriving stock (Show, Generic, Eq, Ord)

instance Semigroup Edit where
  Edit ts ps ws <> Edit ts' ps' ws' = Edit (ts <> ts') (ps <> ps') (ws <> ws')

instance Monoid Edit where
  mempty = Edit [] [] []

type EditM a = MaybeT (Writer Edit) a

runEditM :: EditM () -> [Edit]
runEditM em = case runWriter $ runMaybeT em of
  (Nothing, _) -> []
  (Just (), Edit [] [] []) -> []  -- noop, optimise out
  (Just (), e) -> [e]

bTake :: Board -> Int -> Int -> EditM Value
bTake brd x y =
  case Bimap.lookup x y brd.cells of
    Just v -> do
      lift $ tell $ Edit [(x, y)] [] []
      pure v  -- in bounds and there is a value
    Nothing -> empty

bRead :: Board -> Int -> Int -> EditM Value
bRead brd x y =
  case Bimap.lookup x y brd.cells of
    Just v -> pure v
    Nothing -> empty

bPut :: Int -> Int -> Value -> EditM ()
bPut x y v = lift $ tell $ Edit [] [((x, y), v)] []

bWarp :: Int -> Int -> Int -> Value -> EditM ()
bWarp dt x y val = lift $ tell $ Edit [] [] [Warp dt x y val]

bBinOp :: Int -> Int -> Board -> ((Value, Value) -> Maybe Value) -> EditM ()
bBinOp x y brd op = do  -- todo: use the Alternative instance for some alternatives?
  p <- bTake brd (x-1) y
  q <- bTake brd x (y-1)
  case op (p, q) of
    Nothing -> empty
    Just r -> do
      bPut (x+1) y r
      bPut x (y+1) r

rebuildOpMap :: Cells -> OpMap
rebuildOpMap cells = Map.fromListWith (++)
  [ ((x+dx, y+dy), [(xy, val)])
  | (xy@(x, y), val@(Op op)) <- Bimap.toList cells
  , (dx, dy) <- opInputs op
  ]

-- which cells could trigger these operators,
-- relative to the operator position
opInputs :: Operator -> [(Int, Int)]
opInputs = \case
  OpLeft -> [(1, 0)]
  OpRight -> [(-1, 0)]
  OpUp -> [(0, 1)]
  OpDown -> [(0, -1)]
  OpPlus -> [(-1, 0), (0, -1)]
  OpMinus -> [(-1, 0), (0, -1)]
  OpTimes -> [(-1, 0), (0, -1)]
  OpDiv -> [(-1, 0), (0, -1)]
  OpMod -> [(-1, 0), (0, -1)]
  OpEq -> [(-1, 0), (0, -1)]
  OpNeq -> [(-1, 0), (0, -1)]
  OpWarp -> [(-1, 0), (0, -1), (0, 1), (1, 0)]
  OpSubmit -> []

getEdits :: Bool -> Board -> [Edit]
getEdits disableOpt brd = flip concatMap dirtyOps $ \((x, y), val) -> runEditM $
  case val of
    Op OpLeft -> bTake brd (x+1) y >>= bPut (x-1) y
    Op OpRight -> bTake brd (x-1) y >>= bPut (x+1) y
    Op OpUp -> bTake brd x (y+1) >>= bPut x (y-1)
    Op OpDown -> bTake brd x (y-1) >>= bPut x (y+1)
    Op OpPlus -> bBinOp x y brd $ \case
      (I p, I q) -> Just (I (p+q))
      _ -> Nothing
    Op OpMinus -> bBinOp x y brd $ \case
      (I p, I q) -> Just (I (p-q))
      _ -> Nothing
    Op OpTimes -> bBinOp x y brd $ \case
      (I p, I q) -> Just (I (p*q))
      _ -> Nothing
    Op OpDiv -> bBinOp x y brd $ \case
      (I p, I q) | q /= 0 -> Just (I (p `quot` q))
      _ -> Nothing
    Op OpMod -> bBinOp x y brd $ \case
      (I p, I q) | q /= 0 -> Just (I (p `rem` q))
      _ -> Nothing
    Op OpEq -> bBinOp x y brd $ \case
      (p, q) | p == q -> Just p
      _ -> Nothing
    Op OpNeq -> do
      p <- bTake brd (x-1) y
      q <- bTake brd x (y-1)
      if p /= q
      then do
        bPut (x+1) y q
        bPut x (y+1) p
      else empty
    Op OpWarp -> do
      I dt <- bRead brd x (y+1)
      I dx <- bRead brd (x-1) y
      I dy <- bRead brd (x+1) y
      valWarp <- bTake brd x (y-1)
      bWarp (fromIntegral dt) (x - fromIntegral dx) (y - fromIntegral dy) valWarp
    _ -> empty
  where
    dirtyOps
      | disableOpt = Bimap.toList brd.cells  -- try to reduce all cells
      | otherwise = concatMap (\xy -> Map.findWithDefault [] xy brd.opMap) brd.dirty

findConflictsBy :: (Ord a, Ord b) => (a -> [b]) -> [a] -> Set a
findConflictsBy ixs es = Set.unions [es' | es' <- Map.elems targets, Set.size es' > 1]
  where
    targets = Map.fromListWith (<>) [(xy, Set.singleton e) | e <- es, xy <- ixs e]

data BoardError
  = ConflictingReductions
  deriving stock Show

tick :: Bool -> Board -> Either BoardError (Board, [Warp], Set Value)
tick disableOpt brd =
  case conflicts of
    [] -> Right
      ( Board
        { cells = newCells
        , dirty = if doesMoveOps || doesOverwriteOp
            then allCoords newCells  -- can't use the cache in the next step
            else Set.fromList $ concatMap (\(Edit _ts ps _ws) -> map fst ps) edits
        , opMap = if doesMoveOps || doesOverwriteOp
            then rebuildOpMap newCells
            else brd.opMap
        }
      , concatMap (.warps) edits
      , submits
      )
    _ -> Left ConflictingReductions
  where
    vs `insertManyInto` brd' = foldl' (\b ((x, y), v) -> Bimap.insert x y v b) brd' vs
    edits = getEdits disableOpt brd
    conflicts = Set.toList $ findConflictsBy (map fst . (.puts)) edits
    doesMoveOps = not $ null [() | Edit _ts ps _ws <- edits, (_xy, Op _) <- ps]
    doesOverwriteOp = not $ null [() | Edit _ts ps _ws <- edits, ((x,y), _) <- ps, Just (Op _) <- [Bimap.lookup x y brd.cells]]
    newCells =
      [(xy, v) | e <- edits, (xy, v) <- e.puts]
      `insertManyInto`
      (brd.cells `Bimap.withoutKeys` [xy | e <- edits, xy <- e.takes])
    isBeingTakenAway xy = not $ null
      [ ()
      | Edit ts _ps _ws <- edits
      , xy `elem` ts
      ]
    submits = Set.fromList
      [ val
      | Edit _ts ps _ws <- edits
      , ((x, y), val) <- ps
      , Bimap.lookup x y brd.cells == Just (Op OpSubmit)
      , not $ isBeingTakenAway (x, y)
      ]

type Inputs = [Value]

parseCell :: (Int, Int, ByteString) -> Either ByteString (Maybe ((Int, Int), Value))
parseCell (x, y, txt) = fmap ((x,y),) <$> case txt of
  "." -> pure Nothing
  ""  -> pure Nothing  -- found in CSV files
  ">" -> pure $ Just (Op OpRight)
  "<" -> pure $ Just (Op OpLeft)
  "^" -> pure $ Just (Op OpUp)
  "v" -> pure $ Just (Op OpDown)
  "+" -> pure $ Just (Op OpPlus)
  "-" -> pure $ Just (Op OpMinus)
  "*" -> pure $ Just (Op OpTimes)
  "/" -> pure $ Just (Op OpDiv)
  "%" -> pure $ Just (Op OpMod)
  "@" -> pure $ Just (Op OpWarp)
  "=" -> pure $ Just (Op OpEq)
  "#" -> pure $ Just (Op OpNeq)
  "S" -> pure $ Just (Op OpSubmit)
  "A" -> pure $ Just (Input 0)
  "B" -> pure $ Just (Input 1)
  (readMaybe . BS8.unpack -> Just i)
    | abs i < 100 -> pure $ Just (I i)
    | otherwise   -> Left "Ints must be at most 99 in absolute value"
  _ -> Left $ "unparseable cell: " <> BS8.pack (show txt)

type Format = (Bool, ByteString -> [ByteString])

formatTextNoComments :: Format
formatTextNoComments = (False, BS8.words)

formatText :: Format
formatText = (True, BS8.words)

formatCsv :: Format
formatCsv = (True, BS8.split ',')

parseBoard :: Format -> ByteString -> Either ByteString Board
parseBoard (comments, splitLine) txt = do
  let isComment txt' =
        (comments && "#" `BS8.isPrefixOf` txt')
        || BS8.null txt'
  cells <- Bimap.fromList . catMaybes <$> mapM parseCell
    [ (x, y, word)
    | (y, line) <- zip [1..] $ filter (not . isComment) $ BS8.lines txt
    , (x, word) <- zip [1..] $ splitLine line
    ]
  pure Board
    { cells
    , dirty = allCoords cells
    , opMap = rebuildOpMap cells
    }

allCoords :: Cells -> Set Point
allCoords cells = Set.fromList $ map fst (Bimap.toList cells)

data Box
  = BText ByteString
  | BGrid [[Box]] -- list of columns

render :: Box -> ByteString
render (BText bs) = bs
render (BGrid grid) = ret where
  collen = map (\c -> maximum $ map len c) grid
  len (BText s) = BS8.length s
  len (BGrid _) = error "render: nested grid is not supported"
  rows = transpose grid
  padl l bs = BS8.replicate (l - BS8.length bs) ' ' <> bs
  ret = BS8.unlines
    [ BS8.intercalate " "
      [ padl clen bs
      | (clen, BText bs) <- zip collen row
      ]
    | row <- rows
    ]

class Pretty a where
  pretty :: a -> Box

instance Pretty Operator where
  pretty = BText . \case
    OpLeft -> "<"
    OpRight -> ">"
    OpUp -> "^"
    OpDown -> "v"
    OpPlus -> "+"
    OpMinus -> "-"
    OpTimes -> "*"
    OpDiv -> "/"
    OpMod -> "%"
    OpWarp -> "@"
    OpEq -> "="
    OpNeq -> "#"
    OpSubmit -> "S"

instance Pretty Value where
  pretty = \case
    I i -> BText (BS8.pack $ show i)
    Op op -> pretty op
    Input i -> BText $ BS8.singleton $ chr $ ord 'A' + fromInteger i

instance Pretty Board where
  pretty brd | Bimap.null brd.cells = BText "(empty board)"
  pretty brd = BGrid
    [ [ case Bimap.lookup x y brd.cells of
          Nothing -> BText "."
          Just v -> pretty v
      | y <- [minY..maxY]
      ]
    | x <- [minX..maxX]
    ]
   where
    (minX, maxX, minY, maxY) = Bimap.bounds brd.cells
