module HERMIT.UVector.Life where

import Life.Types
import Data.Vector.Unboxed as Vector
import Data.List as List (nub,(\\))

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Vector Bool)


-- Transformations required by hermit for worker/wrapper conversions
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: Size -> [Pos] -> Vector Bool
repb sz b = generate ((fst sz)*(snd sz)) (\i -> Prelude.elem (i `mod` (fst sz), i `div` (fst sz)) b)

{-# NOINLINE absb #-}
absb :: Size -> Vector Bool -> [Pos]
absb sz b = [ (x,y) | x <- [0..((fst sz) - 1)], y <- [0..((snd sz) - 1)], b ! (y * (fst sz) + x) ]

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard c $ repb (fst c) $ board b
	where c = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard c $ absb (fst c) $ board b
	where c = config b

-- representation of "empty"
repxB f = repB . f

-- abstraction of "empty"
absxB f = absB . f

-- representation of "dims", "alive", "isAlive", "isEmpty"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty"
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
{-# RULES "empty-b" [~] forall c. repB (LifeBoard c []) = LifeBoard c (generate ((fst (fst c))*(snd (fst c))) (\i -> False)) #-}

{-# RULES "alive"  [~] forall b. board (absB b) = absb (fst (config b)) (board b) #-}

{-# RULES "dims" [~] forall b. config (absB b) = config b #-}

{-# RULES "diff-b" [~] forall b1 b2. 
	repB (LifeBoard (config (absB b1)) (board (absB b1) \\ board (absB b2))) 
	= 
	LifeBoard (config b1) (generate (Vector.length (board b1)) (\i -> ((board b1) ! i) /= ((board b2) ! i)))
 #-}

{-# RULES "isAlive" [~] forall p b. 
	Prelude.elem p (board (absB b)) 
	= 
	(board b) ! (((snd p) * (fst (fst (config b)))) + (fst p))
 #-}

{-# RULES "inv" [~] forall b f p. 
	repB (LifeBoard (config (absB b)) (if f (repB (absB b)) p then Prelude.filter ((/=) p) (board (absB b)) else p : board (absB b))) 
	= 
	LifeBoard (config b) (imap (\i v -> if p == (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b)))) then not v else v) (board b))
 #-}

{-# RULES "isEmpty" [~] forall b. repB (absB b) = b #-}

{-# RULES "survivors" [~] forall f b n. 
	repB (LifeBoard (config (absB b)) (Prelude.filter (\p -> Prelude.elem (f (repB (absB b)) p) n) (board (absB b)))) 
	= 
	LifeBoard (config b) (Vector.zipWith (&&) (board b) (generate (Vector.length (board b)) (\i -> Prelude.elem (f b (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b))))) n)))
 #-}

{-# RULES "births" [~] forall f1 f2 f3 b n. 
	repB (LifeBoard (config (absB b)) (Prelude.filter (\p -> f1 (repB (absB b)) p && f2 (repB (absB b)) p == n) (nub (Prelude.concatMap (f3 (config (absB b))) (board (absB b)))))) 
	= 
	LifeBoard (config (absB b)) (generate (Vector.length (board b)) (\i -> let p = (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b)))) in f1 b p && f2 b p == n)) #-}

{-# RULES "nextgen" [~] forall f1 f2 b. 
	repB (LifeBoard (config (absB b)) (board (absB (f1 (repB (absB b)))) Prelude.++ board (absB (f2 (repB (absB b)))))) 
	= 
	LifeBoard (config b) (Vector.zipWith (||) (board (f1 b)) (board (f2 b)))
 #-}


