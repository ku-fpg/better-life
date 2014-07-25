{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module NewLife where

-- Libraries required for Hermit transformations
import Life.Types
import Data.Set as Set
import Data.List (sort)

-- Standard implementation
type Board = LifeBoard [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard (Set Pos)

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

-- abstraction of (Pos -> Set Pos)
absPb :: (Pos -> Set Pos) -> (Pos -> [Pos])
absPb f = absb . f

-- repCPb and absCPb can be used for the neighbs function
-- representation of (Config -> Pos -> [Pos])
repCPb :: (Config -> Pos -> [Pos]) -> (Config -> Pos -> Set Pos)
repCPb f c = repPb (f c)

-- abstraction of (Config -> Pos -> Set Pos)
absCPb :: (Config -> Pos -> Set Pos) -> (Config -> Pos -> [Pos])
absCPb f c = absPb (f c)

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


-- Rules for hermit conversion
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (board b) #-}
{-# RULES "not-elem-absb" [~] forall p b. not (elem p (absb b)) = notMember p b #-}
{-# RULES "elem-absb" [~] forall p b. elem p (absb b) = member p b #-}
{-# RULES "repb-filter" [~] forall f b. repb (Prelude.filter f b) = Set.filter f (repb b) #-}
{-# RULES "repb-map" [~] forall f b. repb (sort (Prelude.map f b)) = Set.map f (repb b) #-}

{-# RULES "absPb-fun" [~] forall f. absPb f = absb . f #-}
{-# RULES "repPb-fun" [~] forall f. repPb f = repb . f #-}
{-# RULES "absBPB-fun" [~] forall f. absBPB f = f . repB #-}
{-# RULES "absCPb-fun" [~] forall f c. absCPb f c = absPb (f c) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}
{-# RULES "filter-absb" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b) #-}
{-# RULES "length-absb" [~] forall b. length (absb b) = size b #-}

--{-# RULES "length-size [~] forall af nf b. length (filter (af (repB b)) (absb (f (config b)  #-}

{-
{-# RULES "repb-filter-absb" [~] forall f b. repb (Prelude.filter f (absb b)) = Set.filter f b #-}
{-# RULES "repb-map-absb" [~] forall f b. repb (sort (Prelude.map f (absb b))) = Set.map f b #-}

{-# RULES "length-absb" [~] forall b. length . Prelude.filter (isAlive (absB b)) . (neighbs (config (absB b))) = size . Set.filter (isAlive b) . (neighbs (config b)) #-}

{-# RULES "survivors" [~] forall b. [ p | p <- board (absB b), elem (liveneighbs (absB b) p) [2,3] ] = Set.filter (\p -> elem (liveneighbs b p) [2,3]) (board b) #-}

{-# RULES "births" [~] forall b. [ p | p <- nub (concat (Prelude.map (neighbs (config (absB b))) (board (absB b)))), isEmpty (absB b) p, liveneighbs (absB b) p == 3 ] = Set.filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) (Set.foldr (\p s -> union s (neighbs (config b) p)) Set.empty (board b)) #-}

{-# RULES "nextgen" [~] forall b. sort (survivors (absB b) ++ births (absB b)) = survivors b `union` births b #-}
-}


