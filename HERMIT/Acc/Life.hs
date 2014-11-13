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
absB b = LifeBoard c $! absb (Prelude.fst c) $ board b
         where c = config b

-- representation of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
repBx :: (Board -> a) -> Board' -> a
repBx f = f . absB

-- abstraction of "dims", "alive", "isAlive", "isEmpty", "liveneighbs"
absBx :: (Board' -> a) -> Board -> a
absBx f = f . repB

-- representation of (Pos -> Board -> Board) "inv"
repxBB :: (a -> Board -> Board) -> a -> Board' -> Board'
repxBB f x = repB . (f x) . absB

-- abstraction of (Pos -> Board' -> Board') "inv"
absxBB :: (a -> Board' -> Board') -> a -> Board -> Board
absxBB f x = absB . (f x) . repB

-- representation of "empty"
repxB :: (a -> Board) -> a -> Board'
repxB f = repB . f

-- abstraction of "empty"
absxB :: (a -> Board') -> a -> Board
absxB f = absB . f

-- representation of (Board -> Board -> Board) "diff"
repBBB :: (Board -> Board -> Board) -> Board' -> Board' -> Board'
repBBB f b = repB . (f (absB b)) . absB

-- abstraction of (Board' -> Board' -> Board') "diff"
absBBB :: (Board' -> Board' -> Board') -> Board -> Board -> Board
absBBB f b = absB . (f (repB b)) . repB

-- representation of (Board -> Board) "births", "survivors", "nextgen", "next"
repBB :: (Board -> Board) -> Board' -> Board'
repBB f = repB . f . absB

-- abstraction of (Board' -> Board') "births", "survivors", "nextgen", "next"
absBB :: (Board' -> Board') -> Board -> Board
absBB f = absB . f . repB

-- Rules for hermit conversion

{-# RULES "empty" [~] repxB (\c -> LifeBoard c []) = (\c -> let sz = Prelude.fst c
                                                            in LifeBoard c (A.fill (A.index2 (lift $ Prelude.fst sz) (lift $ Prelude.snd sz)) 0)) #-}
                                                            
{-# RULES "dims" [~] repBx (\b -> case (config b) of (s,w) -> s) = (\b -> case (config b) of (s,w) -> s) #-}

{-# RULES "diff" [~] repBBB (\b1 b2 -> LifeBoard (config b1) (board b1 List.\\ board b2)) = 
                              (\b1 b2 -> LifeBoard (config b1) $ A.generate (shape (board b1))
                                                                            (\ix -> let Z :. i :. j = unlift ix
                                                                                    in (((board b1) A.! (A.index2 i j)) /=* ((board b2) A.! (A.index2 i j))) ? (1, 0))) #-}

{-# RULES "alive" [~] repBx (\b -> (board b)) = (\b -> let (w,h) = Prelude.fst $ config b
                                                           prs = A.reshape (A.index1 (lift (w * h))) $ A.generate (index2 (lift w) (lift h)) 
                                                                                                                  (\ix -> let Z :. i :. j = unlift ix
                                                                                                                          in lift (i :: Exp Int, j :: Exp Int))
                                                           res = A.filter (\pr -> let (i,j) = unlift pr :: (Exp Int, Exp Int)
                                                                                   in ((board b) A.! (index2 i j)) ==* 1) prs
                                                       in toList $ run res) #-}

{-# RULES "inv" [~] forall f. 
    repPBB (\p b -> LifeBoard (config b) f) 
    = (\(x,y) b -> LifeBoard (config b) $ A.generate (shape (board b))
                                                     (\ix -> let Z :. i :. j = unlift ix
                                                                 val = (board b) A.! (A.index2 i j)
                                                             in (x ==* i &&* y ==* j) ? ((val + 1) `mod` 2, val))) #-}

{-# RULES "next" [~] forall f. repBB (absBB f) = f #-}

{-# RULES "nextgen" [~] forall f1 f2. repBB (\b -> LifeBoard (config b) (List.sort (board (absBB f1 b) List.++ board (absBB f2 b)))) = 
                                            (\b ->  let pattern :: Stencil3x3 Int -> Exp Int
                                                        pattern ((t1,t2,t3), (l,m,r), (bt1, bt2, bt3)) = t1 + t2 + t3 + l + r + bt1 + bt2 + bt3
                                                        liveneighbs :: Board' -> Board'
                                                        liveneighbs b = if Prelude.snd $ config b
                                                                        then LifeBoard (config b) (stencil pattern Wrap (board b))
                                                                        else LifeBoard (config b) $ stencil pattern (Constant 0) (board b)
                                                        survivorsOrBirths :: Exp Int -> Exp Int -> Exp Int
                                                        survivorsOrBirths cell neighbs = (cell ==* 1 &&* (neighbs ==*2 ||* neighbs ==* 3)) ? (1, (cell ==* 0 &&* neighbs ==* 3) ? (1,0))
                                                        b' = liveneighbs b
                                                    in LifeBoard (config b) $ (A.zipWith (survivorsOrBirths) (board b) (board b'))) #-}
