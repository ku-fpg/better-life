module UVector where

import Life.Types
import qualified Data.Vector.Unboxed as V
import Data.List

-- Standard implementation
type Board = LifeBoard [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard (V.Vector Bool)

-- repb and absb change the underlying board field
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: [Pos] -> Size -> V.Vector Bool
repb xs sz = V.fromList [(y,z) `elem` xs | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)]]

{-# NOINLINE absb #-}
absb :: V.Vector Bool -> Size -> [Pos]
absb v sz = [(y,z) | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)], v V.! (z * (snd sz) + y)]

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard (config b) $ repb (board b) (fst $ config b)

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb (board b) (fst $ config b)

-- representation of (Config -> Board) "empty"
repCB :: (Config -> Board) -> (Config -> Board')
repCB f = repB . f

-- abstraction of (Config -> Board') "empty"
absCB :: (Config -> Board') -> (Config -> Board)
absCB f = absB . f

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

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Board -> Board) "nextgen" and "next"
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "nextgen" and "next"
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB

