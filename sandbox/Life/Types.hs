module Life.Types where

-- | 'Pos' is a zero-indexed position in the (abstact) board
type Pos = (Int,Int)
type Size = (Int,Int)
type Config = (Size,Bool)

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
	alive :: b -> [Pos]

scene :: Life board => Config -> [Pos] -> board
scene = foldr inv . empty

-- Runs Life with the given board for the given number of generations
-- 	At the end of the run it returns the final board configuration
runLife :: Life board => Int -> board -> board
runLife 0 b = b
runLife n b = runLife (n-1) (next b)

-- laws
--   config . empty        == id
--   inv pos . inv pos == id
--   scene (config board) (alive board) == board

data LifeBoard b = LifeBoard
		{ config :: Config
		, board :: b }
	deriving Show


