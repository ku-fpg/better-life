{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module NewLife where

-- Libraries required for Hermit transformations
import Life.Types
import Data.Set as Set
import Data.List as List (sort,nub,(\\))

-- Standard implementation
type Board = LifeBoard [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard (Set Pos)

----------------------------------------------------------------------
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

-- representation of (Config -> Board) "empty"
repCB :: (Config -> Board) -> (Config -> Board')
repCB f = repB . f

-- abstraction of (Config -> Board') "empty"
absCB :: (Config -> Board') -> (Config -> Board)
absCB f = absB . f

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

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx f = f . repB

-- representation of (Pos -> [Pos]) "neighbors"
repPb :: (Pos -> [Pos]) -> (Pos -> Set Pos)
repPb f = repb . f

-- abstraction of (Pos -> Set Pos) "neighbors"
absPb :: (Pos -> Set Pos) -> (Pos -> [Pos])
absPb f = absb . f

-- representation of (Config -> Pos -> [Pos]) "neighbs"
repCPb :: (Config -> Pos -> [Pos]) -> (Config -> Pos -> Set Pos)
repCPb f c = repPb (f c)

-- abstraction of (Config -> Pos -> Set Pos) "neighbs"
absCPb :: (Config -> Pos -> Set Pos) -> (Config -> Pos -> [Pos])
absCPb f c = absPb (f c)

-- representation of (Board -> [Pos]) "births" and "survivors"
repBb :: (Board -> [Pos]) -> (Board' -> Set Pos)
repBb f = repb . f . absB

-- abstraction of (Board' -> Set Pos) "births" and "survivors"
absBb :: (Board' -> Set Pos) -> (Board -> [Pos])
absBb f = absb . f . repB

-- representation of (Board -> Board) "nextgen" and "next"
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "nextgen" and "next"
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB


-- Rules for hermit conversion
{-# RULES "not-elem-absb" [~] forall p b. not (elem p (absb b)) = notMember p b #-}
{-# RULES "elem-absb" [~] forall p b. elem p (absb b) = member p b #-}
{-# RULES "repb-filter" [~] forall f b. repb (Prelude.filter f b) = Set.filter f (repb b) #-}
{-# RULES "repb-map" [~] forall f b. repb (sort (Prelude.map f b)) = Set.map f (repb b) #-}
{-# RULES "filter-absb" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b) #-}
{-# RULES "length-absb" [~] forall b. length (absb b) = size b #-}
{-# RULES "ncm-absb" [~] forall f b. nub (concatMap (\p -> absb (f p)) (absb b)) = absb (unions (toList(Set.map (\p -> f p) b))) #-}
{-# RULES "sort++absb" [~] forall b1 b2. sort (absb b1 ++ absb b2) = absb (union b1 b2) #-}

{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (board b) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}
{-# RULES "repb-absb" [~] forall b. repb (absb b) = b #-}
{-# RULES "repB-L-absb" [~] forall c b. repB (LifeBoard c (absb b)) = LifeBoard c b #-}

{-# RULES "lifeboard" [~] forall c. LifeBoard c (repb []) = LifeBoard c Set.empty #-}
{-# RULES "diff-absb" [~] forall b1 b2. absb b1 List.\\ absb b2 = absb (b1 Set.\\ b2) #-}
{-# RULES "sort-absb" [~] forall b p. sort (p : absb b) = absb (insert p b) #-}


