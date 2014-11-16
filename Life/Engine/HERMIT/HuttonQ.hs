{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.HERMIT.HuttonQ where

import Life.Types
import Data.List

import HERMIT.QTree.Life hiding (Board) -- so we have access to abs/rep functions

type Board = LifeBoard Config [Pos]

neighbs :: Config -> Pos -> [Pos]
neighbs c@((w,h),warp) (x,y) = if warp
		then map (\(x,y) -> (x `mod` w, y `mod` h)) neighbors
		else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) neighbors
	where neighbors = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs (config b)

survivors :: Board -> Board
survivors b = LifeBoard (config b) $ filter (\p -> elem (liveneighbs b p) [2,3]) $ board b
--[ p | p <- board b, elem (liveneighbs b p) [2,3] ]

births :: Board -> Board
births b = LifeBoard (config b) $ filter (\p -> isEmpty b p && liveneighbs b p == 3) 
				$ nub $ concatMap (neighbs (config b)) $ board b
--[ p | p <- nub $ concatMap (neighbs (config b)) $ board b, isEmpty b p, liveneighbs b p == 3 ]

nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ board (survivors b) ++ board (births b)

instance Life Board where
	empty c = LifeBoard c []
	dims b = fst $ config b
	diff b1 b2 = LifeBoard (config b1) $ board b1 \\ board b2
	next b = nextgen b
	inv p b = LifeBoard (config b) $ 
		if isAlive b p 
		then filter ((/=) p) $ board b
		else p : board b
	{-# NOINLINE alive #-}
	alive b = board b


