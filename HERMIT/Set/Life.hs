module HERMIT.Set.Life where

-- Libraries required for Hermit transformations
import Life.Types
import Data.Set as Set
import Data.List as List (sort,nub,(\\))

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Set Pos)


-- Transformations required by hermit for worker/wrapper conversions
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: [Pos] -> Set Pos
repb = fromDistinctAscList

{-# NOINLINE absb #-}
absb :: Set Pos -> [Pos]
absb = toAscList

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard (config b) $ repb (board b)

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb (board b)

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx :: (Board -> a) -> Board' -> a
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx :: (Board' -> a) -> Board -> a
absBx f = f . repB

-- representation of "empty"
repxB :: (a -> Board) -> a -> Board'
repxB f = repB . f

-- abstraction of "empty"
absxB :: (a -> Board') -> a -> Board
absxB f = absB . f

-- representation of (Pos -> Board -> Board) "inv"
repxBB :: (a -> Board -> Board) -> a -> Board' -> Board'
repxBB f x = repB . (f x) . absB

-- abstraction of (Pos -> Board' -> Board') "inv"
absxBB :: (a -> Board' -> Board') -> a -> Board -> Board
absxBB f x = absB . (f x) . repB

-- representation of (Board -> Board -> Board) "diff"
repBBB :: (Board -> Board -> Board) -> Board' -> Board' -> Board'
repBBB f b = repB . (f (absB b)) . absB

-- abstraction of (Board' -> Board' -> Board') "diff"
absBBB :: (Board' -> Board' -> Board') -> Board -> Board -> Board
absBBB f b = absB . (f (repB b)) . repB

-- representation of (Board -> Board) "births", "survivors", "nextgen", "next"
repBB :: (Board -> Board) -> Board' -> Board'
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "births", "survivors", "nextgen", "next"
absBB :: (Board' -> Board') -> Board -> Board
absBB f = absB . f . repB


-- Rules for hermit conversion
{-# RULES "empty-b" [~] repxB (\c -> LifeBoard c []) = (\c -> LifeBoard c Set.empty) #-}

{-# RULES "isAlive" [~] repBx (\b p -> elem p (board b)) = (\b p -> member p (board b)) #-}

{-# RULES "inv" [~] forall f. 
	repxBB (\p b -> LifeBoard (config b) (if absBx f b p
					then Prelude.filter ((/=) p) (board b) 
					else sort (p : (board b)))) 
	= (\p b -> LifeBoard (config b) (if f b p 
					then insert p (board b) 
					else delete p (board b))) #-}

{-# RULES "alive" [~] repBx (\b -> (board b)) = (\b -> toAscList (board b)) #-}

{-# RULES "dims" [~] repBx (\b -> case (config b) of (s,w) -> s) = (\b -> case (config b) of (s,w) -> s) #-}

{-# RULES "diff-b" [~] repBBB (\b1 b2 -> LifeBoard (config b1) (board b1 List.\\ board b2)) = (\b1 b2 -> LifeBoard (config b1) (board b1 Set.\\ board b2)) #-}

{-# RULES "isEmpty" [~] forall f. repBx (\b p -> not (f b p)) = (\b p -> notMember p (board b)) #-}

{-# RULES "liveneighbs" [~] forall f1 f2. repBx (\b x -> length (Prelude.filter (absBx f1 b) (f2 (config b) x))) = (\b x -> length (Prelude.filter (f1 b) (f2 (config b) x))) #-}

{-# RULES "survivors" [~] forall f n. repBB (\b -> LifeBoard (config b) (Prelude.filter (\p -> elem (absBx f b p) n) (board b)))  = (\b -> LifeBoard (config b) (Set.filter (\p -> elem (f b p) n) (board b))) #-}

{-# RULES "births" [~] forall f1 f2 f3 n. repBB (\b -> LifeBoard (config b) (Prelude.filter (\p -> absBx f1 b p && absBx f2 b p == n) (nub (concatMap (f3 (config b)) (board b))))) = (\b -> LifeBoard (config b) (Set.filter (\p -> f1 b p && f2 b p == n) (unions (toList (Set.map (fromDistinctAscList . (f3 (config b))) (board b)))))) #-}

{-# RULES "nextgen" [~] forall f1 f2. repBB (\b -> LifeBoard (config b) (sort (board (absBB f1 b) ++ board (absBB f2 b)))) = (\b -> LifeBoard (config b) (union (board (f1 b)) (board (f2 b)))) #-}

{-# RULES "next" [~] forall f. repBB (absBB f) = f #-}


