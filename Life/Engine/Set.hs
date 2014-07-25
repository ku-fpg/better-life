{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.Set where

import Prelude hiding (map,filter,foldr)
import Data.List (sort)
import Data.Set as Set

import Life.Types

type Board = LifeBoard (Set Pos)

neighbors :: Pos -> Set Pos
neighbors (x,y) = fromDistinctAscList $ sort [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

neighbs :: Config -> Pos -> Set Pos
neighbs ((w,h),warp) p = if warp
	then map (\(x,y) -> (x `mod` w, y `mod` h)) $ neighbors p
	else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) $ neighbors p

isAlive :: Board -> Pos -> Bool
isAlive b p = member p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = notMember p $ board b

liveneighbs :: Board -> Pos -> Int
liveneighbs b = size . filter (isAlive b) . (neighbs (config b))

survivors :: Board -> Set Pos
survivors b = filter (\p -> elem (liveneighbs b p) [2,3]) $ board b

births :: Board -> Set Pos
births b = filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) $ 
	unions $ toList $ map (neighbs (config b)) b

nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ survivors b `union` births b

instance Life Board where
	empty c = LifeBoard c Set.empty
	dims b = fst $ config b
	diff b1 b2 = LifeBoard (config b1) $ board b1 \\ board b2
	next = nextgen
	inv p b = LifeBoard (config b) $ 
		if isAlive b p 
		then delete p $ board b
		else insert p $ board b
	alive b = toAscList $ board b


