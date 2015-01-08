module HERMIT.Set.Life where

import Data.Set as Set
import Data.List as List (nub,(\\))

import Life.Types

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Set Pos)


-- Transformations required by hermit for worker/wrapper conversions
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: [Pos] -> Set Pos
repb = fromList

{-# NOINLINE absb #-}
absb :: Set Pos -> [Pos]
absb = toList

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard (config b) $ repb (board b)

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb (board b)

-- rep for "alive", "isAlive", "isEmpty", "liveneighbs"
repBx :: (Board -> a) -> Board' -> a
repBx f = f . absB

-- abs for "alive", "isAlive", "isEmpty", "liveneighbs"
absBx :: (Board' -> a) -> Board -> a
absBx f = f . repB

-- rep for "empty"
repxB :: (a -> Board) -> a -> Board'
repxB f = repB . f

-- abs for "empty"
absxB :: (a -> Board') -> a -> Board
absxB f = absB . f

-- rep for "inv"
repxBB :: (a -> Board -> Board) -> a -> Board' -> Board'
repxBB f x = repB . (f x) . absB

-- abs for "inv"
absxBB :: (a -> Board' -> Board') -> a -> Board -> Board
absxBB f x = absB . (f x) . repB

-- rep for "births", "survivors", "nextgen", "next"
repBB :: (Board -> Board) -> Board' -> Board'
repBB f = repB . f . absB

-- abs for "births", "survivors", "nextgen", "next"
absBB :: (Board' -> Board') -> Board -> Board
absBB f = absB . f . repB


-- GHC Rules for HERMIT ------------------------------------------
-- Simplification rules
{-# RULES 
"repb/absb-fusion" [~] forall b. repb (absb b) = b
"LifeBoard-reduce" [~] forall b. LifeBoard (config b) (board b) = b
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-s" [~] repb [] = Set.empty 
"not-elem/notMember" [~] forall p b. not (elem p (absb b)) = notMember p b
"elem/member" [~] forall p b. elem p (absb b) = member p b
"cons/insert" [~] forall p b. p : (absb b) = absb (insert p b)
"filter/delete" [~] forall p b. Prelude.filter ((/=) p) (absb b) = absb (delete p b)
"filter-l/filter-s" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b)
"nub-concatMap/unions" [~] forall f b. nub (concatMap f (absb b)) = absb (unions (toList (Set.map (fromList . f) b)))
"concat/union" [~] forall b1 b2. absb b1 ++ absb b2 = absb (union b1 b2)
 #-}


