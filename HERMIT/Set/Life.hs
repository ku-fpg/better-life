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

-- rep for "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx :: (Board -> a) -> Board' -> a
repBx f = f . absB

-- abs for "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
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

-- rep for "diff"
repBBB :: (Board -> Board -> Board) -> Board' -> Board' -> Board'
repBBB f b = repB . (f (absB b)) . absB

-- abs for "diff"
absBBB :: (Board' -> Board' -> Board') -> Board -> Board -> Board
absBBB f b = absB . (f (repB b)) . repB

-- rep for "births", "survivors", "nextgen", "next"
repBB :: (Board -> Board) -> Board' -> Board'
repBB f = repB . f . absB

-- abs for "births", "survivors", "nextgen", "next"
absBB :: (Board' -> Board') -> Board -> Board
absBB f = absB . f . repB


-- Rules for hermit conversion
{-# RULES "empty-b" [~] forall c. repB (LifeBoard c []) = LifeBoard c Set.empty #-}

{-# RULES "alive" [~] forall b. board (absB b) = toList (board b) #-}

{-# RULES "dims" [~] forall b. config (absB b) = config b #-}

{-# RULES "diff-b" [~] forall b1 b2. 
	repB (LifeBoard (config (absB b1)) (board (absB b1) List.\\ board (absB b2))) 
	= 
	LifeBoard (config b1) (board b1 Set.\\ board b2) 
 #-}

{-# RULES "isAlive" [~] forall b p. elem p (board (absB b)) = member p (board b) #-}

{-# RULES "inv" [~] forall f b p. 
	repB (LifeBoard (config (absB b)) (if f (repB (absB b)) p 
					then Prelude.filter ((/=) p) (board (absB b)) 
					else p : (board (absB b)))) 
	= LifeBoard (config b) (if f b p 
					then delete p (board b) 
					else insert p (board b)) #-}

{-# RULES "isEmpty" [~] forall f b p. not (f (absB b) p) = notMember p (board b) #-}

{-# RULES "liveneighbs" [~] forall f b x. 
	length (Prelude.filter (f (repB (absB b))) x) = length (Prelude.filter (f b) x) #-}

{-# RULES "survivors" [~] forall f b n. 
	repB (LifeBoard (config (absB b)) (Prelude.filter (\p -> elem (f (repB (absB b)) p) n) (board (absB b)))) 
	= 
	LifeBoard (config b) (Set.filter (\p -> elem (f b p) n) (board b)) 
 #-}

{-# RULES "births" [~] forall f1 f2 f3 b n. 
	repB (LifeBoard (config (absB b)) (Prelude.filter (\p -> f1 (repB (absB b)) p && f2 (repB (absB b)) p == n) (nub (concatMap (f3 (config (absB b))) (board (absB b)))))) 
	= 
	LifeBoard (config b) (Set.filter (\p -> f1 b p && f2 b p == n) (unions (toList (Set.map (fromList . (f3 (config b))) (board b))))) 
 #-}

{-# RULES "nextgen" [~] forall f1 f2 b. 
	repB (LifeBoard (config (absB b)) (board (absB (f1 (repB (absB b)))) ++ board (absB (f2 (repB (absB b)))))) 
	= 
	LifeBoard (config b) (union (board (f1 b)) (board (f2 b))) 
 #-}

{-# RULES "next" [~] forall f b. repB (absB (f (repB (absB b)))) = f b #-}


