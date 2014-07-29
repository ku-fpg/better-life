module HERMIT.UVector.Life where

import Life.Types
import qualified Data.Vector.Unboxed as V
import Data.List

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (V.Vector Bool)

-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: Size -> [Pos] -> V.Vector Bool
repb sz xs = V.fromList [(y,z) `elem` xs | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)]]

{-# NOINLINE absb #-}
absb :: Size -> V.Vector Bool -> [Pos]
absb sz v = [(y,z) | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)], v V.! (z * (snd sz) + y)]

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard conf $ repb (fst conf) $ board b
	where conf = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard conf $ absb (fst conf) $ board b
	where conf = config b

-- representation of "empty", "neighbors"
repxB f = repB . f

-- abstraction of "empty", "neighbors"
absxB f = absB . f

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Config -> Pos -> [Pos]) "neighbs"
repCPB :: (Config -> Pos -> Board) -> (Config -> Pos -> Board')
repCPB f c = repxB (f c)

-- abstraction of (Config -> Pos -> Set Pos) "neighbs"
absCPB :: (Config -> Pos -> Board') -> (Config -> Pos -> Board)
absCPB f c = absxB (f c)

-- representation of (Board -> Board) "nextgen" and "next"
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "nextgen" and "next"
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB

-- representation of (Board -> Board -> Board) "diff"
repBBB :: (Board -> Board -> Board) -> Board' -> Board' -> Board'
repBBB f b = repB . (f (absB b)) . absB

-- abstraction of (Board' -> Board' -> Board') "diff"
absBBB :: (Board' -> Board' -> Board') -> Board -> Board -> Board
absBBB f b = absB . (f (repB b)) . repB

-- representation of (Pos -> Board -> Board) "inv"
repPBB :: (Pos -> Board -> Board) -> Pos -> Board' -> Board'
repPBB f p = repB . (f p) . absB

-- abstraction of (Pos -> Board' -> Board') "inv"
absPBB :: (Pos -> Board' -> Board') -> Pos -> Board -> Board
absPBB f p = absB . (f p) . repB


