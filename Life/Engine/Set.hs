{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.Set where

import Data.Set as Set

import Life.Types

type Board = LifeBoard Config (Set Pos)

neighbs :: Config -> Pos -> [Pos]
neighbs c@((w,h),warp) (x,y) = if warp
		then Prelude.map (\(x,y) -> (x `mod` w, y `mod` h)) neighbors
		else Prelude.filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) neighbors
	where neighbors = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

isAlive :: Board -> Pos -> Bool
isAlive b p = member p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = notMember p $ board b

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . Prelude.filter (isAlive b) . (neighbs (config b))

survivors :: Board -> Board
survivors b = LifeBoard (config b) $ Set.filter (\p -> elem (liveneighbs b p) [2,3]) $ board b

births :: Board -> Board
births b = LifeBoard (config b) 
	$ Set.filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) 
		$ unions $ toList $ Set.map (fromList . (neighbs (config b))) $ board b

instance Life Board where
	empty c = LifeBoard c Set.empty
	alive b = toList $ board b
	inv p b = LifeBoard (config b) $ 
		if isAlive b p 
		then delete p $ board b
		else insert p $ board b
	next b = LifeBoard (config b) $ board (survivors b) `union` board (births b)


