module Life where

import Data.List

import Types
import Worlds

data Board = Board 
		{ cnfg :: Config
		, board :: [Pos] }
	deriving (Show)

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
liveneighbs b = length . filter (isAlive b) . (neighbs (cnfg b))

survivors :: Board -> [Pos]
survivors b = [ p | p <- board b, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- nub (concat (map (neighbs (cnfg b)) (board b))), isEmpty b p, liveneighbs b p == 3 ]

nextgen :: Board -> Board
nextgen b = Board (cnfg b) $ sort $ survivors b ++ births b

instance Life Board where
	empty c = Board c []
	config = cnfg
	diff b1 b2 = Board (cnfg b1) $ board b1 \\ board b2
	next b = nextgen b
	inv p b = Board (cnfg b) $ 
		if isAlive b p 
		then filter ((/=) p) $ board b
		else sort $ p : board b
	alive b = board b

main :: Board
main = runLife 500 $ scene ((20,20),True) glider


