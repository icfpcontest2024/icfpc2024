module TTop.Bimap
  ( Bimap
  , empty, insert, delete, lookup
  , toList, fromList, null, bounds
  , union, withoutKeys, map
  ) where

import Prelude hiding (map, lookup, null)
import Data.Ord
import Data.Function
import Data.IntMap.Strict (IntMap)
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap

data Bimap a = Bimap
  { byX :: IntMap (IntMap a)
  , byY :: IntMap (IntMap a)
  }
  deriving stock (Eq)

empty :: Bimap a
empty = Bimap IntMap.empty IntMap.empty

insert :: Int -> Int -> a -> Bimap a -> Bimap a
insert x y val bm = Bimap
  { byX = IntMap.insertWith IntMap.union x (IntMap.singleton y val) bm.byX
  , byY = IntMap.insertWith IntMap.union y (IntMap.singleton x val) bm.byY
  }

del :: Int -> IntMap a -> Maybe (IntMap a)
del k bm
  | IntMap.null updated = Nothing
  | otherwise = Just updated
  where
    updated = IntMap.delete k bm

delete :: Int -> Int -> Bimap a -> Bimap a
delete x y bm = Bimap
  { byX = IntMap.update (del y) x bm.byX
  , byY = IntMap.update (del x) y bm.byY
  }

lookup :: Int -> Int -> Bimap a -> Maybe a
lookup x y bm =
  IntMap.lookup x bm.byX >>=
  IntMap.lookup y

toList :: Bimap a -> [((Int, Int), a)]
toList bm =
  [ ((x, y), val)
  | (x, byY) <- IntMap.toList bm.byX
  , (y, val) <- IntMap.toList byY
  ]

fromList :: [((Int, Int), a)] -> Bimap a
fromList xyvs = Bimap
  { byX = IntMap.fromList
    [ (x, IntMap.fromList
      [ (y, v)
      | ((_x, y), v) <- g
      ])
    | g <- chunkBy getX xyvs
    , let x = fst . fst $ head g
    ]
  , byY = IntMap.fromList
    [ (y, IntMap.fromList
      [ (x, v)
      | ((x, _y), v) <- g
      ])
    | g <- chunkBy getY xyvs
    , let y = snd . fst $ head g
    ]
  }
  where
    chunkBy f = List.groupBy ((==) `on` f) . List.sortBy (comparing f)
    getX ((x, _y), _v) = x
    getY ((_x, y), _v) = y

null :: Bimap a -> Bool
null bm = IntMap.null bm.byX

bounds :: Bimap a -> (Int, Int, Int, Int)
bounds bm | null bm =
  (maxBound, minBound, maxBound, minBound)
bounds bm =
  ( fst $ IntMap.findMin bm.byX
  , fst $ IntMap.findMax bm.byX
  , fst $ IntMap.findMin bm.byY
  , fst $ IntMap.findMax bm.byY
  )

union :: Bimap a -> Bimap a -> Bimap a
union p q = Bimap
  { byX = IntMap.unionWith IntMap.union p.byX q.byX
  , byY = IntMap.unionWith IntMap.union p.byY q.byY
  }

withoutKeys :: Bimap a -> [(Int, Int)] -> Bimap a
withoutKeys bm xys = Bimap
  { byX = List.foldl' (\bx (x, y) -> IntMap.update (del y) x bx) bm.byX xys
  , byY = List.foldl' (\by (x, y) -> IntMap.update (del x) y by) bm.byY xys
  }

map :: (a -> b) -> Bimap a -> Bimap b
map f bm = Bimap
  { byX = IntMap.map (IntMap.map f) bm.byX
  , byY = IntMap.map (IntMap.map f) bm.byY
  }
