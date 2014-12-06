module HERMIT.QTree.Life where

import Life.Types
import Data.QuadTree
import Data.Boolean (xor)
import Data.List ((\\),sort,nub)

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (QuadTree Bool)


-- Transformations required by hermit for worker/wrapper conversions
{-# NOINLINE indices #-}
indices :: Size -> [Pos]
indices (w,h) = [ (x,y) | x<-[0..w-1], y<-[0..h-1] ]

-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: Size -> [Pos] -> QuadTree Bool
repb sz = foldr (\p qt -> setLocation p qt True) $ makeTree sz False

{-# NOINLINE absb #-}
absb :: QuadTree Bool -> [Pos]
absb qt = [ p | p <- indices (treeDimensions qt), getLocation p qt ]

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard c $ repb (fst c) $ board b
	where c = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb $ board b

-- representation of "empty"
repxB f = repB . f

-- abstraction of "empty"
absxB f = absB . f

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Pos -> Board -> Board) "inv"
repxBB :: (a -> Board -> Board) -> a -> Board' -> Board'
repxBB f x = repB . (f x) . absB

-- abstraction of (Pos -> Board' -> Board') "inv"
absxBB :: (a -> Board' -> Board') -> a -> Board -> Board
absxBB f x = absB . (f x) . repB

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


-- Rules for hermit conversion
{-# RULES "empty-b" [~] forall c. repB (LifeBoard c []) =  LifeBoard c (makeTree (fst c) False) #-}

{-# RULES "alive"  [~] forall b. board (absB b) = absb (board b) #-}

{-# RULES "dims" [~] forall b. config (absB b) = config b #-}

{-# RULES "diff-b" [~] forall b1 b2. 
	repB (LifeBoard (config (absB b1)) ((board (absB b1)) \\ (board (absB b2)))) 
	= 
	LifeBoard (config b1) (foldr (\p qt-> setLocation p qt (getLocation p (board b1) `xor` getLocation p (board b2))) (makeTree (fst (config b1)) False) (indices (fst (config b1))))
 #-}
 
{-# RULES "isAlive" [~] forall p b. elem p (board (absB b)) = getLocation p (board b) #-}

{-# RULES "inv" [~] forall b f p. 
	repB (LifeBoard (config (absB b)) (if f (repB (absB b)) p then filter ((/=) p) (board (absB b)) else (:) p (board (absB b))))
	= 
	LifeBoard (config b) (setLocation p (board b) (not (getLocation p (board b))))
 #-}

{-# RULES "isEmpty" [~] forall b. repB (absB b) = b #-}

{-# RULES "liveneighbs" [~] forall f b x. 
	length (Prelude.filter (f (repB (absB b))) x) = length (Prelude.filter (f b) x) #-}

{-# RULES "survivors" [~] forall f b n. 
	repB (LifeBoard (config (absB b)) (filter (\p -> elem (f (repB (absB b)) p) n) (board (absB b))))
	= 
	LifeBoard (config b) (foldr (\p qt -> setLocation p qt (getLocation p (board b) && elem (f b p) n)) (makeTree (fst (config b)) False) (indices (fst (config b))))
 #-}

{-# RULES "births" [~] forall f1 f2 f3 b n. 
	repB (LifeBoard (config (absB b)) (filter (\p -> (f1 (repB (absB b)) p) && ((f2 (repB (absB b)) p) == n)) (nub (concatMap (f3 (config (absB b))) (board (absB b)))))) 
	= 
	LifeBoard (config b) (foldr (\p qt -> setLocation p qt (f1 b p && f2 b p == n)) 
			(makeTree (fst (config b)) False) 
			(indices (fst (config b))))
 #-}

{-# RULES "nextgen" [~] forall b f1 f2. 
	repB (LifeBoard (config (absB b)) (board (absB (f1 (repB (absB b)))) ++ board (absB (f2 (repB (absB b)))))) 
	= 
	LifeBoard (config b) (foldr (\p qt -> setLocation p qt (getLocation p (board (f1 b)) || getLocation p (board (f2 b)))) (makeTree (fst (config b)) False) (indices (fst (config b))))
 #-}


