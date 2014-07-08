module NewLife (Board', repB, absB) where

import Data.Set as Set

import Types

-- Libraries required for Hermit transformations
import Data.Function (fix)
import Life

data Board' = Board' 
	{ cnfg :: Config,
	 board :: Set Pos }

repB :: Board -> Board'
repB b = Board' (cnfg b) $ fromDistinctAscList (board b)

absB :: Board' -> Board
absB b = Board (cnfg b) $ toAscList (board b)

-- repCPb and absCPb can be used for the neighbs function
-- representation of (Config -> Pos -> [Pos])
repCPb :: (Config -> Pos -> [Pos]) -> (Config -> Pos -> Set Pos)
repCPb f c = repB . (f c)

-- abstraction of (Config -> Pos -> Set Pos)
absCPb :: (Config -> Pos -> Set Pos) -> (Config -> Pos -> [Pos])
absCPb f c = absB . (f c)

-- repBPB and absBPB can be used for isAlive and isEmpty
-- representation of (Board -> Pos -> Bool)
repBPB :: (Board -> Pos -> Bool) -> (Board' -> Pos -> Bool)
repBPB f b = f (absB b)

-- abstraction of (Board' -> Pos -> Bool)
absBPB :: (Board' -> Pos -> Bool) -> (Board -> Pos -> Bool)
absBPB f b = f (repB b)

-- representation of (Board -> Pos -> Int) "liveneighbs"
repBPI :: (Board -> Pos -> Int) -> (Board' -> Pos -> Int)
repBPI f b = f (absB b)

-- abstraction of (Board' -> Pos -> Int) "liveneighbs"
absBPI :: (Board' -> Pos -> Int) -> (Board -> Pos -> Int)
absBPI f b = f (repB b)

-- repEBB and absEBB can be used for births, survivors, and nextgen
-- representation of (Board -> Board)
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB

-- abstraction of (Board' -> Board')
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB

-- Rules for data structure conversion
{-# RULES "isAlive" [~0] forall b p. elem p (absB b) = Set.member p b #-}
{-# RULES "isEmpty" [~0] forall b p. not (elem p (absB b)) = Set.notMember p b #-}

-- NOT SURE ABOUT THE FOLLOWING RULES?????????????????
{-# RULES "liveneighbs" [~0] forall b p. length . filter (elem p (absB b)) . (neighbs (cnfg (absB b))) = size . Set.filter (Set.member p b) . (neighbs (cnfg b)) #-}

{-# RULES "survivors" [~0] forall b. [ p | p <- board (absB b), elem (liveneighbs (absB b) p) [2,3] ] = Set.filter (\p -> elem (liveneighbs b p) [2,3]) (board b) #-}

{-# RULES "births" [~0] forall b. [ p | p <- nub (concat (map (neighbs (cnfg (absB b))) (board (absB b)))), not (elem p (absB b)), liveneighbs (absB b) p == 3 ] = Set.filter (\p -> (isEmpty b p) && (liveneighbs b p == 3)) (Set.foldr (\p s -> union s (neighbs (cnfg b) p)) Set.empty (board b)) #-}

{-# RULES "nextgen" [~0] forall b. sort $ survivors (absB b) ++ births (absB b) = survivors b `union` births b #-}

{- Rules for instance definitions??
{-# RULES "empty" [~0] forall c. absB [] = Set.empty #-}
{-# RULES "diff" [~0] forall b1 b2. board (absB b1) \\ board (absB b2) = Set.(\\) (board b1) (board b2) #-}
{-# RULES "inv" [~0] forall p b. if elem p (absB b) then filter ((/=) p) (board b) else sort (p : board b) = if Set.member p b then delete p (board b) else insert p (board b) #-}
-}

-- Needed because the fusion rule we generate isn't too useful yet.
{-# RULES "repB-absB-fusion" [~0] forall b. repB (absB b) = b #-}


