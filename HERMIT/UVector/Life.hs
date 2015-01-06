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


-- Rules for moving transformers
{-# RULES 
"board/absB" [~] forall b. board (absB b) = absb (fst (config b)) (board b) 
"LifeBoard/absb" [~] forall c b. LifeBoard c (absb (fst c) b) = absB (LifeBoard c b)
"repB/LifeBoard" [~] forall c b. repB (LifeBoard c b) = LifeBoard c (repb (fst c) b)
 #-}

-- Rules for eliminating transformers
{-# RULES 
"repB/absB" [~] forall b. repB (absB b) = b 
"config/absB" [~] forall b. config (absB b) = config b 
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-v" [~] forall s. 
	repb s [] = generate (fst s * snd s) (\i -> False)
"diff/generate" [~] forall b1 b2. 
	absb (fst (config b1)) (board b1) \\ absb (fst (config b2)) (board b2) = 
	absb (fst (config b1)) (generate (Vector.length (board b1)) 
								     (\i -> ((board b1) ! i) /= ((board b2) ! i)))
"elem/lookup" [~] forall p b s. 
	Prelude.elem p (absb s (board b)) = board b ! (snd p * fst s + fst p)
"cons/imap" [~] forall p s b. 
	p : absb s b = 
	absb s (imap (\i v -> if (not v) && p == (i `mod` fst s, i `div` fst s) then not v else v) b)
"filter/imap" [~] forall p s b. 
	Prelude.filter ((/=) p) (absb s b) = 
	absb s (imap (\i v -> if v && p == (i `mod` fst s, i `div` fst s) then not v else v) b)
"if-replace" [~] forall s x p b. 
	absb s (if x then imap (\i v -> if v && p == (i `mod` fst s, i `div` fst s) then not v else v) b 
				else imap (\i v -> if (not v) && p == (i `mod` fst s, i `div` fst s) then not v else v) b) = 
	absb s (imap (\i v -> if p == (i `mod` fst s, i `div` fst s) then not v else v) b)
"filter/zipWith" [~] forall f b n s. 
	Prelude.filter (\p -> Prelude.elem (f b p) n) (absb s (board b)) = 
	absb s (Vector.zipWith (&&) (board b) (generate (Vector.length (board b)) (\i -> Prelude.elem (f b (i `mod` fst s, i `div` fst s)) n)))
"filter/generate" [~] forall f1 f2 f3 b n s. 
	Prelude.filter (\p -> f1 b p && f2 b p == n) (nub (Prelude.concatMap (f3 (config b)) (absb s (board b)))) = 
	absb s (generate (Vector.length (board b)) (\i -> let p = (i `mod` fst s, i `div` fst s) in f1 b p && f2 b p == n))
"concat/zipWith" [~] forall b f1 f2.
	board (absB (f1 (repB (absB b)))) Prelude.++ board (absB (f2 (repB (absB b)))) = 
	absb (fst (config b)) (Vector.zipWith (||) (board (f1 (repB (absB b)))) (board (f2 (repB (absB b)))))
 #-}


