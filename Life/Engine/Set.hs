module Life.Engine.Set where

import Data.List (sort)
import Data.Set as Set

import Life.Types

data Board = Board 
		{ cnfg :: Config,
		 board :: Set Pos }
	deriving (Show)

neighbors :: Pos -> Set Pos
neighbors (x,y) = fromDistinctAscList $ sort [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

neighbs :: Config -> Pos -> Set Pos
neighbs ((w,h),warp) p = 
		if warp
		then Set.map (\(x,y) -> (x `mod` w, y `mod` h)) $ neighbors p
		else Set.filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) $ neighbors p

isAlive :: Board -> Pos -> Bool
isAlive b p = member p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = notMember p $ board b

liveneighbs :: Board -> Pos -> Int
liveneighbs b = size . Set.filter (isAlive b) . (neighbs (cnfg b))

survivors :: Board -> Set Pos
survivors b = Set.filter (\p -> elem (liveneighbs b p) [2,3]) $ board b

births :: Board -> Set Pos
births b = Set.filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) $ 
	Set.foldr (\p s -> union s (neighbs (cnfg b) p)) Set.empty $ board b

nextgen :: Board -> Board
nextgen b = Board (cnfg b) $ survivors b `union` births b

instance Life Board where
	empty c = Board c Set.empty
	config = cnfg
	diff b1 b2 = Board (cnfg b1) $ board b1 \\ board b2
	next = nextgen
	inv p b = Board (cnfg b) $ 
		if isAlive b p 
		then delete p $ board b
		else insert p $ board b
	alive b = toAscList $ board b


