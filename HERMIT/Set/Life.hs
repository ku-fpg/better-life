module HERMIT.Set.Life where

import Data.Set as Set
import Data.List as List (nub,(\\))

import Life.Types

-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Set Pos)


-- Transformations required by hermit for worker/wrapper conversions
-- repb and absb change the underlying board field
{-# NOINLINE repb #-}
repb :: [Pos] -> Set Pos
repb = fromList

{-# NOINLINE absb #-}
absb :: Set Pos -> [Pos]
absb = toList

-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard (config b) $ repb (board b)

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard (config b) $ absb (board b)

-- rep for "alive"
repBx :: (Board -> Scene) -> Board' -> Scene
repBx f b = f (absB b)

-- abs for "alive"
absBx :: (Board' -> Scene) -> Board -> Scene
absBx f b = f (repB b)

--"isAlive", "isEmpty"
repBpb :: (Board -> Pos -> Bool) -> Board' -> Pos -> Bool
repBpb f b p = f (absB b) p

absBpb :: (Board' -> Pos -> Bool) -> Board -> Pos -> Bool
absBpb f b p = f (repB b) p

--"liveneighbs"
repBpi :: (Board -> Pos -> Int) -> Board' -> Pos -> Int
repBpi f b p = f (absB b) p

absBpi :: (Board' -> Pos -> Int) -> Board -> Pos -> Int
absBpi f b p = f (repB b) p

-- rep for "empty"
repcB :: (Config -> Board) -> Config -> Board'
repcB f c = repB (f c)

-- abs for "empty"
abscB :: (Config -> Board') -> Config -> Board
abscB f c = absB (f c)

-- rep for "inv"
reppBB :: (Pos -> Board -> Board) -> Pos -> Board' -> Board'
reppBB f x b = repB (f x (absB b))

-- abs for "inv"
abspBB :: (Pos -> Board' -> Board') -> Pos -> Board -> Board
abspBB f x b = absB (f x (repB b))

-- rep for "births", "survivors", "next"
repBB :: (Board -> Board) -> Board' -> Board'
repBB f b = repB (f (absB b))

-- abs for "births", "survivors", "next"
absBB :: (Board' -> Board') -> Board -> Board
absBB f b = absB (f (repB b))


-- GHC Rules for HERMIT 
-- Simplification rules
{-# RULES 
"repb/absb-fusion" [~] forall b. repb (absb b) = b
"LifeBoard-reduce" [~] forall b. LifeBoard (config b) (board b) = b
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-s" [~] repb [] = Set.empty 
"not-elem/notMember" [~] forall p b. not (elem p (absb b)) = notMember p b
"elem/member" [~] forall p b. elem p (absb b) = member p b
"cons/insert" [~] forall p b. p : (absb b) = absb (insert p b)
"filter/delete" [~] forall p b. Prelude.filter ((/=) p) (absb b) = absb (delete p b)
"filter-l/filter-s" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b)
"nub-concatMap/unions" [~] forall f b. nub (concatMap f (absb b)) = absb (unions (toList (Set.map (fromList . f) b)))
"concat/union" [~] forall b1 b2. absb b1 ++ absb b2 = absb (union b1 b2)
 #-}


