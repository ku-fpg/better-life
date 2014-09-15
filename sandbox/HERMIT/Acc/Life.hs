{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, ScopedTypeVariables #-}
module HERMIT.Acc.Life where

import Life.Types
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
import Data.Array.Accelerate.CUDA

import qualified Data.List as List


-- Standard implementation
type Board = LifeBoard Config [Pos]
-- The new data structure to be used in the implementation
type Board' = LifeBoard Config (Acc (Array DIM2 Int))

repb :: Size -> [Pos] -> Acc (Array DIM2 Int)
repb (w,h) xs = A.reshape (A.index2 (lift w) (lift h)) (A.scatter to def src)
                where sz = List.length xs
                      to = A.map (\pr -> let (x,y) = unlift pr
                                         in (x * (lift w)) + y)
                                 (A.use $ A.fromList (Z :. sz) xs)
                      src = A.fill (A.index1 (lift sz)) 1
                      def = A.fill (A.index1 (lift $ w * h)) 0

absb :: Size -> Acc (Array DIM2 Int) -> [Pos]
absb (w,h) arr = let prs = A.reshape (A.index1 (lift (w * h))) $ A.generate (index2 (lift w) (lift h)) 
                                                                            (\ix -> let Z :. i :. j = unlift ix
                                                                                    in lift (i :: Exp Int, j :: Exp Int))
                     res = A.filter (\pr -> let (i,j) = unlift pr :: (Exp Int, Exp Int)
                                            in (arr A.! (index2 i j)) ==* 1) prs
                 in toList $ run res


-- repB and absB change the entire Board structure
{-# NOINLINE repB #-}
repB :: Board -> Board'
repB b = LifeBoard c $ repb (Prelude.fst c) $ board b
         where c = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard c $ absb (Prelude.fst c) $ board b
         where c = config b

-- representation of (Board -> Board) "nextgen"
repBB :: (Board -> Board) -> (Board' -> Board')
repBB f = repB . f . absB
-- abstraction of (Board' -> Board') "nextgen" 
absBB :: (Board' -> Board') -> (Board -> Board)
absBB f = absB . f . repB

-- representation of "empty"
repxB f = repB . f
-- abstraction of "empty"
absxB f = absB . f
-- representation of "dims", "alive"
repBx f = f . absB
-- abstraction of "dims", "alive"
absBx f = f . repB
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
{-# RULES "LifeBoard-absb" [~] forall c b. LifeBoard c (absb (Prelude.fst c) b) = absB (LifeBoard c b) #-}
{-# RULES "LifeBoard-absb-config" [~] forall b c v. LifeBoard (config b) (absb (Prelude.fst c) v) = absB (LifeBoard (config b) v) #-}
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (Prelude.fst (config b)) (board b) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}


-- Convert empty
{-# RULES "repB-null" [~] forall c. repB (LifeBoard c []) = let sz = Prelude.fst c
                                                            in LifeBoard c (A.fill (A.index2 (lift $ Prelude.fst sz) (lift $ Prelude.snd sz)) 0) #-}

-- Convert inv
{-# RULES "if-absb" [~] forall a b c f p. LifeBoard c (if a 
                                                       then Prelude.filter f (absb (Prelude.fst c) (board b)) 
                                                       else List.sort ((:) p (absb (Prelude.fst c) (board b)))) =  
                                                       let (x,y) = p
                                                       in absB (LifeBoard c $ A.generate (shape (board b))
                                                                                        (\ix -> let Z :. i :. j = unlift ix
                                                                                                    val = (board b) A.! (A.index2 i j)
                                                                                                in ((lift x) ==* i &&* (lift y) ==* j) ? ((val + 1) `mod` 2, val)))#-}

-- Convert diff
{-# RULES "diff-absb" [~] forall c b1 b2. repB (LifeBoard c ((List.\\) (absb (Prelude.fst c) (board b1))  
                                                                       (absb (Prelude.fst (config b2)) (board b2)))) = 
                                                LifeBoard c (A.generate (shape (board b1))
                                                                        (\ix -> let Z :. i :. j = unlift ix
                                                                                in (((board b1) A.! (A.index2 i j)) /=* ((board b2) A.! (A.index2 i j))) ? (1, 0))) #-}
-- Convert nextgen
{-# RULES "seq-par" [~] forall c sz b1 f1 f2 f3 . repB (LifeBoard c (List.sort ((Prelude.++) 
                                                                                (board (LifeBoard c (Prelude.filter f1 (absb sz (board b1)))))
                                                                                (board (LifeBoard c (Prelude.filter f2 (List.nub (Prelude.concatMap f3 (absb sz (board b1))))))))))=  
 
                                                  let pattern :: Stencil3x3 Int -> Exp Int
                                                      pattern ((t1,t2,t3), (l,m,r), (bt1, bt2, bt3)) = t1 + t2 + t3 + l + r + bt1 + bt2 + bt3
                                                      liveneighbs :: Board' -> Board'
                                                      liveneighbs b = if Prelude.snd $ config b
                                                                      then LifeBoard (config b) (stencil pattern Wrap (board b))
                                                                      else LifeBoard (config b) $ stencil pattern (Constant 0) (board b)
                                                      survivorsOrBirths :: Exp Int -> Exp Int -> Exp Int
                                                      survivorsOrBirths cell neighbs = (cell ==* 1 &&* (neighbs ==*2 ||* neighbs ==* 3)) ? (1, (cell ==* 0 &&* neighbs ==* 3) ? (1,0))
                                                      b' = liveneighbs b1
                                                  in LifeBoard c $ (A.zipWith (survivorsOrBirths) (board b1) (board b')) #-}

