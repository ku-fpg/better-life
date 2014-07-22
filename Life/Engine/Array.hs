{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.Array where

import Data.Array
import Data.List (sort,nub,(\\))

import Life.Types

type Board = LifeBoard (Array Pos Bool)

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

neighbs :: Config -> Pos -> [Pos]
neighbs ((w,h),warp) p = if warp
		then map (\(x,y) -> (x `mod` w, y `mod` h)) $ neighbors p
		else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) $ neighbors p

isAlive :: Board -> Pos -> Bool
isAlive b p = board b ! p

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (config b))

survivors :: Board -> [Pos]
survivors b = [ p | (p,e) <- assocs $ board b, e, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- nub $ concatMap (neighbs (config b)) ps, isEmpty b p, liveneighbs b p == 3 ]
	where ps = [ p | (p,e) <- assocs $ board b, e ]

nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ array bnds [ (i,False) | i <- range bnds ] // [ (i,True) | i <- sort $ survivors b ++ births b ]
	where bnds = bounds $ board b

instance Life Board where
	empty c@((w,h),_) = LifeBoard c $ array ((0,0),(w-1,h-1)) [ ((x,y),False) | (x,y) <- range ((0,0),(w-1,h-1)) ]
	dims b = fst $ config b
	diff b1 b2 = let c@((w,h),_) = config b1 in
		LifeBoard (config b1) $ array ((0,0),(w-1,h-1)) 
			[ ((x,y),False) | (x,y) <- range ((0,0),(w-1,h-1)) ]
			// 
			[ (i,True) | (i,_) <- (assocs (board b1) \\ assocs (board b2)) ]
	next = nextgen
	inv p b = LifeBoard (config b) $ board b // 
		if isAlive b p 
		then [(p,False)]
		else [(p,True)]
	alive b = [ p | (p,e) <- assocs $ board b, e ]

