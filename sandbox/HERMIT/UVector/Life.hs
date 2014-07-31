module HERMIT.UVector.Life where

import Life.Types
import Data.Vector.Unboxed as Vector
import Data.List as List (nub,sort,(\\))

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Vector Bool)


-- Transformations required by hermit for worker/wrapper conversions
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: Size -> [Pos] -> Vector Bool
repb sz xs = fromList [(y,z) `Prelude.elem` xs | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)]]

{-# NOINLINE absb #-}
absb :: Size -> Vector Bool -> [Pos]
absb sz v = [ (y,z) | y <- [0..(fst sz) - 1], z <- [0..(snd sz) - 1], v ! (z * (snd sz) + y) ]

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard c $ repb (fst c) $ board b
	where c = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard c $ absb (fst c) $ board b
	where c = config b

-- representation of "empty", "neighbors"
repxB f = repB . f

-- abstraction of "empty", "neighbors"
absxB f = absB . f

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Config -> Pos -> [Pos]) "neighbs"
repCPB :: (Config -> Pos -> Board) -> (Config -> Pos -> Board')
repCPB f c = repxB (f c)

-- abstraction of (Config -> Pos -> Set Pos) "neighbs"
absCPB :: (Config -> Pos -> Board') -> (Config -> Pos -> Board)
absCPB f c = absxB (f c)

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
{-# RULES "repB-LifeBoard" [~] forall c b. repB (LifeBoard c b) = LifeBoard c (repb (fst c) b) #-}
{-# RULES "LifeBoard-absb" [~] forall c b. LifeBoard c (absb (fst c) b) = absB (LifeBoard c b) #-}
{-# RULES "LifeBoard-absb-config" [~] forall b c v. LifeBoard (config b) (absb (fst c) v) = absB (LifeBoard (config b) v) #-}
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (fst (config b)) (board b) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}

-- Rules that convert list-based combinators into vector-based combinators
-- For conversion to Vector.empty
{-# RULES "repB-null" [~] forall c. repB (LifeBoard c []) = LifeBoard c Vector.empty #-}
-- For conversion to Vector.!
{-# RULES "elem-absb" [~] forall p c b. Prelude.elem p (absb (fst c) b) = b ! ((snd p)*(snd (fst c)) + (fst p)) #-}
-- For conversion to Vector.\\
{-# RULES "diff-absb" [~] forall b1 b2. (absb (fst (config b1)) (board b1)) \\ (absb (fst (config b2)) (board b2)) = absb (fst (config b1))(generate (Vector.length (board b1)) (\i -> ((board b1) ! i) /= ((board b2) ! i))) #-}
-- For conversion to Vector.filter
{-# RULES "filter-id" [~] forall f b c. Prelude.filter f (absb (fst c) b) = absb (fst c) (Vector.filter id b) #-}
-- For conversion to Vector.length
{-# RULES "length-absb" [~] forall b c. Prelude.length (absb (fst c) b) = Vector.length b #-}
-- For conversion to Vector.generate for survivors function
{-# RULES "filter-sur" [~] forall f b n. Prelude.filter (\p -> Prelude.elem (f b p) n) (absb (fst (config b)) (board b)) = absb (fst (config b)) (generate (Vector.length (board b)) (\i -> Prelude.elem (f b (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b))))) n)) #-}
-- For nub-concatMap chain in births function
{-# RULES "ncm-absb" [~] forall f b. nub (Prelude.concatMap (\p -> absb (fst (config (f (config b) p))) (board (f (config b) p))) (absb (fst (config b)) (board b))) = absb (fst (config b)) (Prelude.foldr1 (Vector.zipWith (||)) [ board (f (config b) (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b))))) | i <- [0..Vector.length (board b) - 1] ]) #-}
-- For conversion to Vector.generate in births function
{-# RULES "filter-bir" [~] forall f1 f2 b n v. Prelude.filter (\p -> f1 b p && f2 b p == n) (absb (fst (config b)) v) = absb (fst (config b)) (generate (Vector.length v) (\i ->  let p = (i `mod` (fst (fst (config b))), i `div` (fst (fst (config b)))) in f1 b p && f2 b p == n)) #-}
-- For conversion to Vector.zipWith in nextgen function
{-# RULES "sort-++-absb" [~] forall b1 b2. sort (absb (fst (config b1)) (board b1) Prelude.++ absb (fst (config b2)) (board b2)) = absb (fst (config b1)) (Vector.zipWith (||) (board b1) (board b2)) #-}
-- For conversion to Vector.update in inv function
{-# RULES "if-absb" [~] forall a b f p. LifeBoard (config b) (if a then Prelude.filter f (absb (fst (config b)) (board b)) else sort ((:) p (absb (fst (config b)) (board b)))) = let i = fst (fst (config b)) * fst p + snd p in absB (LifeBoard (config b) ((//) (board b) [(i, not (board b ! i))])) #-}


