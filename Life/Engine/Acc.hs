{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, ScopedTypeVariables #-}
module Life.Engine.Acc where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import Life.Types

type Board = LifeBoard Config (Acc (A.Array DIM2 Int))

instance Life Board where
    empty c@(sz, flag) = LifeBoard c (A.fill (A.index2 (lift $ Prelude.fst sz) (lift $ Prelude.snd sz)) 0)
    next b = let pattern :: Stencil3x3 Int-> Exp Int 
                pattern  ((t1, t2, t3),
                          (l , m, r),
                          (b1, b2, b3)) = t1 + t2 + t3 + l + r + b1 + b2 + b3
                liveneighbs :: Board -> Board
                liveneighbs b = case (Prelude.snd $ config b) of
                                     True -> LifeBoard (config b) $ stencil pattern Wrap (board b)
                                     False -> LifeBoard (config b) $ stencil pattern (Constant 0) (board b)
                                    
                survivorsOrBirths :: Exp Int -> Exp Int -> Exp Int
                survivorsOrBirths cell neighs = (cell ==* 1 &&* (neighs ==* 2 ||* neighs ==* 3)) ? (1, (cell ==* 0 &&* neighs ==* 3) ? (1,0))
                b' = liveneighbs b
            in LifeBoard (config b) $ A.zipWith (survivorsOrBirths) (board b) (board b')
    inv (x,y) b = LifeBoard (config b) $ A.generate (shape (board b))
                                                (\ix -> let Z :. i :. j = unlift ix
                                                            val = (board b) A.! (A.index2 i j)
                                                        in ((lift x) ==* i &&* (lift y) ==* j) ? ((val + 1) `mod` 2, val))
    alive b = let (w,h) = Prelude.fst $ config b
                  prs = A.reshape (A.index1 (lift (w * h))) $ A.generate (index2 (lift w) (lift h)) 
                                                                         (\ix -> let Z :. i :. j = unlift ix
                                                                                    in lift (i :: Exp Int, j :: Exp Int))
                  res = A.filter (\pr -> let (i,j) = unlift pr :: (Exp Int, Exp Int)
                                         in ((board b) A.! (index2 i j)) ==* 1) prs
              in toList $ run res
