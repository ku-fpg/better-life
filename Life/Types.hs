module Life.Types where

-- | 'Pos' is a zero-indexed position in the (abstact) board
type Pos = (Int,Int)
type Size = (Int,Int)
type Config = (Size,Bool)

class Life board where
  -- create
  empty :: Config -> board
  -- board operations
  diff :: board -> board -> board
  next :: board -> board
  -- point operations
  inv :: Pos   -> board -> board
  -- projections
  config :: board -> Config
  alive :: board -> [Pos]

scene :: Life board => Config -> [Pos] -> board
scene = foldr inv . empty

-- Runs Life with the given board for the given number of generations
-- 	At the end of the run it returns the final board configuration
runLife :: Life board => Int -> board -> board
runLife 0 b = b
runLife n b = runLife (n-1) (next b)

-- laws
--   size . empty        == id
--   flip pos . flip pos == id
--   scene (size board) (alive board) == board


