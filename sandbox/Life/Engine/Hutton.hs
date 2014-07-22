{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.Hutton where

import Data.List

import Life.Types

type Board = LifeBoard [Pos]

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

neighbs :: Config -> Pos -> [Pos]
neighbs ((w,h),warp) p = sort $ 
		if warp
		then map (\(x,y) -> (x `mod` w, y `mod` h)) $ neighbors p
		else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) $ neighbors p

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (config b))

survivors :: Board -> [Pos]
survivors b = [ p | p <- board b, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- nub $ concatMap (neighbs (config b)) $ board b, isEmpty b p, liveneighbs b p == 3 ]

nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ sort $ survivors b ++ births b

instance Life Board where
	empty c = LifeBoard c []
	dims b = fst $ config b
	diff b1 b2 = LifeBoard (config b1) $ board b1 \\ board b2
	next b = nextgen b
	inv p b = LifeBoard (config b) $ 
		if isAlive b p 
		then filter ((/=) p) $ board b
		else sort $ p : board b
	alive b = board b


