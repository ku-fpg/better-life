module Life.Engine.Array where

import Data.Array
import Data.List (sort,nub,(\\))

import Life.Types

data Board = Board 
		{ cnfg :: Config,
		 board :: Array Pos Bool }
	deriving (Show)

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
liveneighbs b = length . filter (isAlive b) . (neighbs (cnfg b))

survivors :: Board -> [Pos]
survivors b = [ p | (p,e) <- assocs $ board b, e, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- nub $ concatMap (neighbs (cnfg b)) ps, isEmpty b p, liveneighbs b p == 3 ]
	where ps = [ p | (p,e) <- assocs $ board b, e ]

nextgen :: Board -> Board
nextgen b = Board (cnfg b) $ array bnds [ (i,False) | i <- range bnds ] // [ (i,True) | i <- sort $ survivors b ++ births b ]
	where bnds = bounds $ board b

instance Life Board where
	empty c@((w,h),_) = Board c $ array ((0,0),(w,h)) [ ((x,y),False) | (x,y) <- range ((0,0),(w,h)) ]
	config = cnfg
	diff b1 b2 = Board (cnfg b1) $ board (empty (cnfg b1)) // 
			[ (i,True) | (i,_) <- (assocs (board b1) \\ assocs (board b2)) ]
	next = nextgen
	inv p b = Board (cnfg b) $ board b // 
		if isAlive b p 
		then [(p,False)]
		else [(p,True)]
	alive b = [ p | (p,e) <- assocs $ board b, e ]

