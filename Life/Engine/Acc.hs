{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators #-}
module Life.Engine.Acc where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import Life.Types

type Board = LifeBoard Config (Acc (A.Array DIM2 Int))


glid :: Board
glid = LifeBoard ((5,5), False) $ (use  $ A.fromList (Z :. 5 :. 5) [1, 0, 0, 0, 0,
                                                                    0, 0, 0, 0, 0,
                                                                    0, 0, 0, 1, 0,
                                                                    0, 0, 0, 0, 1,
                                                                    0, 0, 1, 1, 1])


nextgen :: Board -> Board 
nextgen b = let b' = liveneighbs b
            in LifeBoard (config b) $ A.zipWith (survivorsOrBirths) (board b) (board b')
            where liveneighbs :: Board -> Board
                  liveneighbs b = if (Prelude.snd $ config b)
                                  then LifeBoard (config b) $ stencil pattern Wrap (board glid)
                                  else LifeBoard (config b) $ stencil pattern (Constant 0) (board glid)
                                    where pattern :: Stencil3x3 Int-> Exp Int 
                                          pattern  ((t1, t2, t3),
                                                    (l , m, r),
                                                    (b1, b2, b3)) = t1 + t2 + t3 + l + r + b1 + b2 + b3
                  survivorsOrBirths :: Exp Int -> Exp Int -> Exp Int
                  survivorsOrBirths cell neighs = (cell ==* 1 &&* (neighs ==* 2 ||* neighs ==* 3)) ? (1, (cell ==* 0 &&* neighs ==* 3) ? (1,0))
                  
                  b' = liveneighbs b



