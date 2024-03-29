module Arr
  ( Arr
  , Arr.lookup
  , (!)
  , Arr.map
  , toList
  , fromList
  , update
  , adjust
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

fromList :: Arr a -> [(Int, a)]
fromList (Arr c) = IntMap.toList c

update :: Int -> a -> Arr a -> Arr a
update i a (Arr c) = Arr $ IntMap.update (const (Just a)) i c

adjust :: Int -> (a -> a) -> Arr a -> Arr a
adjust i f (Arr c) = Arr $ IntMap.adjust f i c

elems :: [a] -> Arr a
elems = Arr . IntMap.fromList . zip [0..]
