module Life.Types where

-- | 'Pos' is a zero-indexed position in the (abstact) board
type Pos = (Int,Int) 

class Life board where
  -- create
  empty :: (Int,Int) -> board
  -- board operations
  diff  :: board -> board -> board
  next  :: board -> board
  -- point operations
  inv   :: Pos   -> board -> board
  -- projectors
  size  :: board -> (Int,Int)
  alive :: board -> [Pos]

-- laws
--   size . empty        == id
--   flip pos . flip pos == id
--   scene (size board) (alive board) == board

scene :: Life board => (Int,Int) -> [Pos] -> board
scene = foldr inv . empty

