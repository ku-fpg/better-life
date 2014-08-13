module HERMIT.QTree.Life where

import Life.Types
--import Prelude hiding (foldr,foldr1,foldl,foldl1,and,or,any,all,sum,product,concat,concatMap,maximum,minimum)
--import qualified Data.Foldable as Foldable
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
absB b = LifeBoard c $ absb $ board b
	where c = config b

-- representation of "empty"
repxB f = repB . f

-- abstraction of "empty"
absxB f = absB . f

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Config -> Pos -> Board) "neighbs"
repCPB :: (Config -> Pos -> Board) -> (Config -> Pos -> [Pos])
repCPB f c = board . (f c)

-- abstraction of (Config -> Pos -> [Pos]) "neighbs"
absCPB :: (Config -> Pos -> [Pos]) -> (Config -> Pos -> Board)
absCPB f c = (LifeBoard c) . (f c)

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


-- Rules for hermit conversion
-- Rules that move abs and rep functions up/down the AST
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (board b) #-}
{-# RULES "LifeBoard-absb" [~] forall c b. LifeBoard c (absb b) = absB (LifeBoard c b) #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}
-- For removing an unnecessary build of LifeBoard
{-# RULES "board-LifeBoard" [~] forall c b. board (LifeBoard c b) = b #-}

-- Rules that convert list-based combinators into QuadTree-based combinators
{-# RULES "repB-null" [~] forall c. repB (LifeBoard c []) =  LifeBoard c (makeTree (fst c) False) #-}
-- For conversion of diff function
{-# RULES "diff-absb" [~] forall b1 b2. 
	(absb (board b1)) \\ (absb (board b2)) 
	= 
	let cb = config b1
	in let sz = fst cb
	in absb (foldr (\p qt-> setLocation p qt (getLocation p (board b1) `xor` getLocation p (board b2))) 
		(makeTree sz False) 
		(indices sz))
 #-}
-- For conversion of isAlive function
{-# RULES "elem-absb" [~] forall p b. elem p (absb b) = getLocation p b #-}
-- For conversion of survivors function
{-# RULES "filter-sur" [~] forall f b n. 
	filter (\p -> elem (f (repB (absB b)) p) n) (absb (board b))
	= 
	absb (let sz = fst (config b)
		in foldr (\p qt -> setLocation p qt (getLocation p qt && elem (f b p) n)) (makeTree sz False) (indices sz))
 #-}
-- For conversion of births function
{-# RULES "filter-bir" [~] forall f f1 f2 b n. 
	filter (\p -> f1 b p && f2 b p == n) (nub (concatMap f (absb (board b)))) 
	= 
	absb (let cb = config b
		in let sz = fst cb
		in foldr (\p qt -> setLocation p qt (f1 b p && f2 b p == n)) 
			(makeTree sz False) 
			(indices sz))
 #-}
-- For conversion of nextgen function
{-# RULES "sort-++-absb" [~] forall b1 b2. 
	sort (absb (board b1) ++ absb (board b2)) 
	= 
	absb (let cb = config b1 
		in let sz = fst cb 
		in foldr (\p qt -> setLocation p qt (getLocation p (board b1) 
							|| getLocation p (board b2))) 
		(makeTree sz False) 
		(indices sz))
 #-}
-- For conversion to Vector.update in inv function
{-# RULES "if-absb" [~] forall a b c f p. 
	LifeBoard c (if a then Prelude.filter f (absb (board b)) else sort ((:) p (absb (board b)))) 
	= 
	let bb = board b 
	in LifeBoard (config b) (absb (setLocation p bb (not (getLocation p bb))))
 #-}
{-
{-# RULES "LifeBoard-absb-config" [~] forall b c v. LifeBoard (config b) (absb (fst c) v) = absB (LifeBoard (config b) v) #-}

-}

