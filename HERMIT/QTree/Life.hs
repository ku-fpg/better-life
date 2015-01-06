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


-- GHC Rules for HERMIT ------------------------------------------
-- Rules for moving transformers
{-# RULES 
"board/absB" [~] forall b. board (absB b) = absb (board b) 
"LifeBoard/absb" [~] forall c b. LifeBoard c (absb b) = absB (LifeBoard c b)
 #-}

-- Rules for eliminating transformers
{-# RULES 
"repB/absB" [~] forall b. repB (absB b) = b 
"config/absB" [~] forall b. config (absB b) = config b 
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-t" [~] forall c. 
	repB (LifeBoard c []) = LifeBoard c (makeTree (fst c) False)
"diff/foldr" [~] forall b1 b2. 
	absb b1 \\ absb b2 = 
	absb (foldr (\p qt -> setLocation p qt (xor (getLocation p b1) 
											    (getLocation p b2))) 
		        (makeTree (treeDimensions b1) False) 
		        (indices (treeDimensions b1)))
"elem/getLocation" [~] forall p b. elem p (absb b) = getLocation p b
"cons/setLocation" [~] forall p b. p : (absb b) = absb (setLocation p b True)
"filter/setLocation" [~] forall p b. filter ((/=) p) (absb b) = absb (setLocation p b False)
"if-replace" [~] forall v p b. 
	absb (if v then setLocation p b False else setLocation p b True) = 
	absb (setLocation p b (not (getLocation p b)))
"filter/foldr-s" [~] forall f b. 
	filter f (absb b) = 
	absb (foldr (\p qt -> setLocation p qt (getLocation p b && f p)) 
			    (makeTree (treeDimensions b) False) 
			    (indices (treeDimensions b)))
"filter/foldr-b" [~] forall f1 f2 b.
	filter f1 (nub (concatMap f2 (absb b))) = 
	absb (foldr (\p qt -> setLocation p qt (f1 p)) 
				(makeTree (treeDimensions b) False) 
				(indices (treeDimensions b)))
"concat/foldr" [~] forall f1 f2 b.
	absb (board (f1 (repB (absB b)))) ++ absb (board (f2 (repB (absB b)))) = 
	absb (foldr (\p qt -> setLocation p qt (getLocation p (board (f2 (repB (absB b)))))) 
				(board (f1 (repB (absB b))))
				(indices (treeDimensions (board (repB (absB b))))))
 #-}


