module Main where

import Life.Types 
import Life.Console
import Life.Worlds

import Data.List

data Board = Board 
	{ env  :: Env,
	 board :: [Pos] }

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

neighbs :: Env -> Pos -> [Pos]
neighbs ((width,height),warp) (x,y) = 
	sort $ filter (\(x1,y1) -> (x > 0 && x <= width) && (y > 0 && y <= height))
	     $ map warp [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (env b))

survivors :: Board -> [Pos]
survivors b = [ p | p <- board b, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [ p | p <- neighbors, isEmpty b p, liveneighbs b p == 3 ]
	where neighbors = nub $ concat $ map (neighbs (env b)) $ board b

nextgen :: Board -> Board
nextgen b = Board (env b) $ survivors b ++ births b

instance Life Board where
  empty s = Board (s,id) []
  emptyWith e = Board e []
  size = fst . env
  diff b1 b2 = Board (env b1) (board b1 \\ board b2)
  next b = nextgen b
  inv p b | isAlive b p = Board (env b) $ filter ((/=) p) $ board b
		  | otherwise = Board (env b) $ sort $ p : board b
  alive b = board b

life :: Env -> [Pos] -> IO ()
life e b = lifeConsole (sceneWith e b :: Board)

gSize = (20,20)
ggSize = (50,50)

testOriginal = life (gSize, torusSurface gSize) glider

testFlatGlider = life (gSize,flatSurface) glider

testGliderGun = life (ggSize,torusSurface ggSize) gliderGun

testFlatGliderGun = life (ggSize,flatSurface) gliderGun


