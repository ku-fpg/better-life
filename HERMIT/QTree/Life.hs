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

-- representation of "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "alive", "isAlive", "isEmpty", "liveneighbs"
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


-- GHC Rules for HERMIT 
-- Simplification rules
{-# RULES 
"repb/absb-fusion" [~] forall s b. 
	repb s (absb b) = b
"LifeBoard-reduce" [~] forall b. 
	LifeBoard (config b) (board b) = b
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-t" [~] forall s. 
	repb s [] = makeTree s False
"elem/getLocation" [~] forall p b. 
	elem p (absb b) = getLocation p b
"cons/setLocation" [~] forall p b. 
	p : (absb b) = absb (setLocation p b True)
"filter/setLocation" [~] forall p b. 
	filter ((/=) p) (absb b) = absb (setLocation p b False)
"if-replace" [~] forall v p b. 
	absb (setLocation p b (if v then False else True)) = 
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
"concat/foldr" [~] forall f1 f2 b s.
	LifeBoard (config b) (repb s (absb (board (f1 b)) ++ (absb (board (f2 b))))) = 
	LifeBoard (config b) (repb s (absb (foldr (\p qt -> setLocation p qt (getLocation p (board (f1 b)) || getLocation p (board (f2 b)))) 
											(makeTree s False)
											(indices s))))
 #-}


