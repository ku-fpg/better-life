module HuttonEngine where

import Life.Types
import Life.Worlds

import Data.List

data Board = Board 
	{ env  :: Env,
	 board :: [Pos] }

instance Show Board where
  show = show . board

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

neighbs :: Env -> Pos -> [Pos]
neighbs ((w,h),warp) (x,y) = 
	sort $ case warp of
		False -> filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) neighbors
		True -> map (\(x,y) -> (x `mod` w, y `mod` h)) neighbors
	where neighbors = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (env b))

survivors :: Board -> [Pos]
survivors b = [ p | p <- board b, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- n, isEmpty b p, liveneighbs b p == 3 ]
	where n = nub $ concat $ map (neighbs (env b)) $ board b

nextgen :: Board -> Board
nextgen b = Board (env b) $ sort $ survivors b ++ births b

instance Life Board where
  empty e = Board e []
  size = fst . env
  diff b1 b2 = Board (env b1) (board b1 \\ board b2)
  next b = nextgen b
  inv p b 
	| isAlive b p = Board (env b) $ filter ((/=) p) $ board b
	| otherwise = Board (env b) $ sort $ p : board b
  alive b = board b

