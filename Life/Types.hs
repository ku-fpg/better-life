module Life.Types where

-- | 'Pos' is a zero-indexed position in the (abstact) board
type Pos = (Int,Int)
type Size = (Int,Int)
type Warp = Pos -> Pos
type Env = (Size,Warp)

class Life board where
  -- create
  empty :: Size -> board
  emptyWith :: Env -> board
  -- board operations
  diff :: board -> board -> board
  next :: board -> board
  -- point operations
  inv :: Pos   -> board -> board
  -- getters
  size :: board -> Size
  alive :: board -> [Pos]

scene :: Life board => Size -> [Pos] -> board
scene = foldr inv . empty

sceneWith :: Life board => Env -> [Pos] -> board
sceneWith = foldr inv . emptyWith

-- laws
--   size . empty        == id
--   flip pos . flip pos == id
--   scene (size board) (alive board) == board


