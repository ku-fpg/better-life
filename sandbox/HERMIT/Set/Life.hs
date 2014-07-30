{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HERMIT.Set.Life where

-- Libraries required for Hermit transformations
import Life.Types
import Data.Set as Set
import Data.List as List (sort,nub,(\\))

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Set Pos)

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
repCPB f = repxB . f

-- abstraction of (Config -> Pos -> Set Pos) "neighbs"
absCPB :: (Config -> Pos -> Board') -> (Config -> Pos -> Board)
absCPB f = absxB . f

-- representation of (Board -> Board) "births", "survivors", "nextgen", "next"
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "births", "survivors", "nextgen", "next"
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
{-# RULES "repB-LifeBoard" [~] forall c b. repB (LifeBoard c b) = LifeBoard c (repb b) #-}
{-# RULES "LifeBoard-absb" [~] forall c b. LifeBoard c (absb b) = absB (LifeBoard c b) #-}
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (board b) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}

-- Rules that convert list-based combinators into set-based combinators
-- For conversion to Set.empty
{-# RULES "repb-null" [~] forall c. LifeBoard c (repb []) = LifeBoard c Set.empty #-}
-- For conversion to Set.notMember)
{-# RULES "not-elem-absb" [~] forall p b. not (elem p (absb b)) = notMember p b #-}
-- For conversion to Set.member
{-# RULES "elem-absb" [~] forall p b. elem p (absb b) = member p b #-}
-- For conversion to Set.size
{-# RULES "length-absb" [~] forall b. length (absb b) = size b #-}
-- For conversion to Set.filter
{-# RULES "filter-absb" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b) #-}
-- For conversion to Set.union
{-# RULES "sort-++-absb" [~] forall b1 b2. sort (absb b1 ++ absb b2) = absb (union b1 b2) #-}
-- For nub-concatMap chain in births function
{-# RULES "ncm-absb" [~] forall f b. nub (concatMap (\p -> absb (board (f p))) (absb b)) = absb (unions (toList (Set.map (\p -> board (f p)) b))) #-}
-- For conversion to Set.\\
{-# RULES "diff-absb" [~] forall b1 b2. absb b1 List.\\ absb b2 = absb (b1 Set.\\ b2) #-}
-- For conversion to Set.insert
{-# RULES "insertion" [~] forall b p. sort (p : absb b) = absb (insert p b) #-}
-- For conversion to Set.delete
{-# RULES "deletion" [~] forall b p. Prelude.filter ((/=) p) (absb b) = absb (delete p b) #-}

