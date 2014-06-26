module Life.Types where

-- | 'Pos' is a zero-indexed position in the (abstact) board
type Pos = (Int,Int)
type Size = (Int,Int)
type Warp = Pos -> Pos
type Env = (Size,Warp)

class Life board where
  -- create
  empty :: Env -> board
  -- board operations
  diff  :: board -> board -> board
  next  :: board -> board
  -- point operations
  inv   :: Pos   -> board -> board
  -- projectors
  size  :: board -> Size
  alive :: board -> [Pos]

-- laws
--   size . empty        == id
--   flip pos . flip pos == id
--   scene (size board) (alive board) == board

scene :: Life board => Env -> [Pos] -> board
scene = foldr inv . empty

