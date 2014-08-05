{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.UVector where

import Data.Vector.Unboxed as Vector
import Life.Types

type Board = LifeBoard Config (Vector Bool)

neighbs :: Config -> Pos -> Board
neighbs c@((w,h),warp) (x,y) = LifeBoard c $ fromList [ Prelude.elem (px,py) neighbors | px <- [0..w-1], py <- [0..h-1]]
	where 	n = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]
		neighbors = if warp
			then Prelude.map (\(x,y) -> (x `mod` w, y `mod` h)) n
			else Prelude.filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) n
                               
isAlive :: Board -> Pos -> Bool
isAlive b (x,y) = board b ! (x * (fst $ fst $ config b) + y)

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = (foldl' (\acc arg -> if arg then acc + 1 else acc) 0) . (Vector.zipWith (&&) (board b)) . board . (neighbs (config b))

survivors :: Board -> Board
survivors b = LifeBoard (config b) $ Vector.zipWith (&&) cellsWLN (board b) 
	where cellsWLN = generate (Vector.length (board b)) (\i -> Prelude.elem (liveneighbs b (i `div` (fst (fst (config b))), i `mod` (fst (fst (config b))))) [2,3])
                                                                      
births :: Board -> Board
births b = LifeBoard (config b) $ generate 
				(Vector.length (board b)) 
				(\i -> let p = (i `div` (fst (fst (config b))), i `mod` (fst (fst (config b)))) 
					in (isEmpty b p) && (liveneighbs b p == 3))
                                                                         
nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ Vector.zipWith (||) (board (survivors b)) $ board $ births b

instance Life Board where
	empty c@((w,h),warp) = LifeBoard c $ generate (w * h) (\i -> False)
	dims b = fst $ config b
	diff b1 b2 = LifeBoard (config b1) $ generate (Vector.length (board b1)) (\i -> ((board b1) ! i) /= ((board b2) ! i))
	next b = nextgen b
	inv p b = LifeBoard (config b) $ (board b) // [(i, not (board b ! i))]
			where i = fst (fst (config b)) * fst p + snd p
	alive b = [ (x,y) | x <- [0..fst (fst (config b)) - 1], y <- [0.. snd (fst (config b)) - 1], (board b) ! (x * fst (fst (config b)) + y) ]


