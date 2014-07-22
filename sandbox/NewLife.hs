{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module NewLife where

-- Libraries required for Hermit transformations
import Life.Types
import Data.Set as Set

-- The new data structure to be used in the implementation
--import Life.Engine.Hutton
type Board = LifeBoard [Pos] -- Board type from Life/Engine/Hutton.hs
type Board' = LifeBoard (Set Pos)  -- The new Board type we want

instance Life Board' where
	empty c = LifeBoard c Set.empty
	dims b = fst $ config b
	diff b1 b2 = LifeBoard (config b1) $ board b1 \\ board b2
	next b = undefined
	inv p b = LifeBoard (config b) $ 
		if member p $ board b
		then delete p $ board b
		else insert p $ board b
	alive b = toAscList $ board b


-- Transformations required by hermit for worker/wrapper conversions

-- repb and absb change the underlying board field
repb :: [Pos] -> Set Pos
repb = fromDistinctAscList

absb :: Set Pos -> [Pos]
absb = toAscList

-- repB and absB change the entire Board structure
repB :: Board -> Board'
repB b = LifeBoard (config b) $ repb (board b)

absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb (board b)

-- repPb and absPb can be used for neighbors function
-- representation of (Pos -> [Pos])
repPb :: (Pos -> [Pos]) -> (Pos -> Set Pos)
repPb f = repb . f

absPb :: (Pos -> Set Pos) -> (Pos -> [Pos])
absPb f = absb . f

-- repCPb and absCPb can be used for the neighbs function
-- representation of (Config -> Pos -> [Pos])
repCPb :: (Config -> Pos -> [Pos]) -> (Config -> Pos -> Set Pos)
repCPb f c = repb . (f c)

-- abstraction of (Config -> Pos -> Set Pos)
absCPb :: (Config -> Pos -> Set Pos) -> (Config -> Pos -> [Pos])
absCPb f c = absb . (f c)

-- repBPB and absBPB can be used for isAlive and isEmpty
-- representation of (Board -> Pos -> Bool)
repBPB :: (Board -> Pos -> Bool) -> (Board' -> Pos -> Bool)
repBPB f = f . absB

-- abstraction of (Board' -> Pos -> Bool)
absBPB :: (Board' -> Pos -> Bool) -> (Board -> Pos -> Bool)
absBPB f = f . repB

-- representation of (Board -> Pos -> Int) "liveneighbs"
repBPI :: (Board -> Pos -> Int) -> (Board' -> Pos -> Int)
repBPI f = f . absB

-- abstraction of (Board' -> Pos -> Int) "liveneighbs"
absBPI :: (Board' -> Pos -> Int) -> (Board -> Pos -> Int)
absBPI f = f . repB

-- repBb and absBb can be used for births and survivors
-- representation of (Board -> [Pos])
repBb :: (Board -> [Pos]) -> (Board' -> Set Pos)
repBb f = repb . f . absB

-- abstraction of (Board' -> Set Pos)
absBb :: (Board' -> Set Pos) -> (Board -> [Pos])
absBb f = absb . f . repB

-- repBB and absBB can be used for nextgen
-- representation of (Board -> Board)
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board')
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB


-- Needed because the fusion rule we generate isn't too useful yet.
{-# RULES "repB-absB-fusion" [~] forall b. repB (absB b) = b #-}

-- Rules for hermit conversion
{-# RULES "neighbors" [~] forall x y. repb [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)] = fromDistinctAscList $ sort [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)] #-}
{-
{-# RULES "neighbs" [~] forall w h warp p. repb (sort (if warp then Prelude.map (\(x,y) -> (x `mod` w, y `mod` h)) (neighbors p) else Prelude.filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) (neighbors p))) = if warp then Set.map (\(x,y) -> (x `mod` w, y `mod` h)) (neighbors p) else Set.filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) (neighbors p) #-}

{-# RULES "isAlive" [~] forall b p. elem p (board (absB b)) = Set.member p (board b) #-}
{-# RULES "isEmpty" [~] forall b p. not (elem p (board (absB b))) = Set.notMember p (board b) #-}

{-# RULES "liveneighbs" [~] forall b. length . Prelude.filter (isAlive (absB b)) . (neighbs (config (absB b))) = size . Set.filter (isAlive b) . (neighbs (config b)) #-}

{-# RULES "survivors" [~] forall b. [ p | p <- board (absB b), elem (liveneighbs (absB b) p) [2,3] ] = Set.filter (\p -> elem (liveneighbs b p) [2,3]) (board b) #-}

{-# RULES "births" [~] forall b. [ p | p <- nub (concat (Prelude.map (neighbs (config (absB b))) (board (absB b)))), isEmpty (absB b) p, liveneighbs (absB b) p == 3 ] = Set.filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) (Set.foldr (\p s -> union s (neighbs (config b) p)) Set.empty (board b)) #-}

{-# RULES "nextgen" [~] forall b. sort (survivors (absB b) ++ births (absB b)) = survivors b `union` births b #-}
-}

