module Life.Types where

import Data.List (nub)

type Size = (Int,Int)
type Config = (Size,Bool)
type Pos = (Int,Int)
type Scene = [Pos]

data LifeBoard c b = LifeBoard
		{ config :: c
		, board :: b }
	deriving Show

class Life b where
	-- create
	empty :: Config -> b
	-- board operations
	diff :: b -> b -> b
	next :: b -> b
	-- point operations
	inv :: Pos -> b -> b
	-- projections
	dims :: b -> Size
	alive :: b -> Scene

scenes :: [Scene] -> Scene
scenes s = nub $ concat s

scene :: Life board => Config -> Scene -> board
scene c@((w,h),warp) = 
	foldr inv (empty c) . (if warp
		then map (\(x,y) -> (x `mod` w, y `mod` h))
		else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h))) 

-- Runs Life with the given board for the given number of generations
-- 	At the end of the run it returns the final board configuration
runLife :: Life board => Int -> board -> board
runLife 0 b = b
runLife n b = runLife (n-1) (next b)


