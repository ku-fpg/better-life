module HERMIT.Set2.Life where

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
"repB/absB-fusion" [~] forall b. repB (absB b) = b
"config-absB" [~] forall b. config (absB b) = config b
"board-absB" [~] forall b. board (absB b) = absb (board b)
"LifeBoard-absB" [~] forall c b. LifeBoard c (absb b) = absB (LifeBoard c b)
 #-}

--Code replacement rules
{-# RULES 
"empty-l/empty-s" [~] forall c. repB (LifeBoard c []) = LifeBoard c Set.empty 
"not-elem/notMember" [~] forall p b. not (elem p (board (absB b))) = notMember p (board b)
"elem/member" [~] forall p b. elem p (board (absB b)) = member p (board b)
"cons/insert" [~] forall p b. p : (absb (board b)) = absb (insert p (board b))
"filter/delete" [~] forall p b. Prelude.filter ((/=) p) (absb (board b)) = absb (delete p (board b))
"filter-l/filter-s" [~] forall f b. Prelude.filter f (absb b) = absb (Set.filter f b)
"nub-concatMap/unions" [~] forall f b. nub (concatMap f (board (absB b))) = absb (unions (toList (Set.map (fromList . f) (board b))))
"concat/union" [~] forall b1 b2. board (absB b1) ++ board (absB b2) = absb (union (board b1) (board b2))
 #-}


