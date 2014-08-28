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
repB b = LifeBoard c $ repb (fst c) $ board b
         where c = config b

{-# NOINLINE absB #-}
absB :: Board' -> Board
absB b = LifeBoard c $ absb (fst c) $ board b
         where c = config b

-- Rules for hermit conversion
-- Rules that move abs and rep functions up/down the AST
{-# RULES "LifeBoard-absb" [~] forall c b. LifeBoard c (absb (fst c) b) = absB (LifeBoard c b) #-}
{-# RULES "LifeBoard-absb-config" [~] forall b c v. LifeBoard (config b) (absb (fst c) v) = absB (LifeBoard (config b) v) #-}
{-# RULES "board-absB"  [~] forall b. board (absB b) = absb (fst (config b)) (board b) #-}
{-# RULES "config-absB" [~] forall b. config (absB b) = config b #-}
{-# RULES "repB-absB" [~] forall b. repB (absB b) = b #-}
