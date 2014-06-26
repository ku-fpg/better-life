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

instance Life Life.Board where
  empty e = Board e []
  size = fst . env
  diff b1 b2 = Board (env b1) (board b1 \\ board b2)
  next b = nextgen b
  inv p b | isAlive b p = Board (env b) $ filter ((/=) p) $ board b
		  | otherwise = Board (env b) $ sort $ p : board b
  alive b = board b

life :: Int -> Int -> String -> String -> IO ()
life w h ws bs = 
	let warp = case ws of
		"torus-surface" -> (\(x,y) -> (x `mod` w, y `mod` h))
		"flat" -> id
		otherwise -> id
	in let board = case bs of
			"glider gun" -> gliderGun
			"glider" -> glider
			otherwise -> []
		in lifeConsole $ scene ((Size w h),warp) board

main :: IO ()
main = life 20 20 "torus-surface" "glider"

