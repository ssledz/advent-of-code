module Arr
  ( Arr
  , Arr.lookup
  , (!)
  , Arr.map
  , toList
  , fromList
  , update
  , elems
  , size
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

newtype Arr a = Arr (IntMap a) deriving Show

lookup :: Int -> Arr a -> Maybe a
lookup i (Arr c) = IntMap.lookup i c

(!) :: Arr a -> Int -> a
(!) (Arr c) i = c IntMap.! i

size :: Arr a -> Int
size (Arr c) = IntMap.size c

map :: (a -> b) -> Arr a -> Arr b
map f (Arr c) = Arr (IntMap.map f c)

toList :: Arr a -> [a]
toList (Arr c) = IntMap.elems c

elems :: Arr a -> [(Int, a)]
elems (Arr c) = IntMap.toList c

update :: Arr a -> Int -> a -> Arr a
update (Arr c) i a = Arr $ IntMap.update (const (Just a)) i c

fromList :: [a] -> Arr a
fromList = Arr . IntMap.fromList . zip [0..]
