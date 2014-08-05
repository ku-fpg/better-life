module Life.Engine.UVector where

import qualified Data.Vector.Unboxed as V
import Life.Types

type Board = LifeBoard Config (V.Vector Bool)

glider :: [Pos]
glider = [(2,3),(3,4),(4,2),(4,3),(4,4)]

repb :: Size -> [Pos] -> V.Vector Bool
repb sz xs = V.fromList [(y,z) `Prelude.elem` xs | y <- [0..((fst sz) - 1)], z <- [0..((snd sz) - 1)]]

test :: Board
test = (LifeBoard ((5,5), False) (repb (5,5) glider))

isAlive :: Board -> Pos -> Bool
isAlive b (x,y) = let ind = (x * (fst $ fst $ config b) + y)
                  in (board b) V.! ind

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Config -> Pos -> Board
neighbs c@((w,h),warp) (x,y) = LifeBoard c $ repb (w,h) $ if warp
                                                          then map (\(x,y) -> (x `mod` w, y `mod` h)) neighbors
                                                          else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) neighbors
                               where neighbors = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]
                               
liveneighbs :: Board -> Pos -> Int
liveneighbs b p = V.foldl' (\acc arg -> if arg then acc + 1 else acc) 0 $ V.zipWith (&&) (board b) $ board  (neighbs (config b) p)

survivors :: Board -> Board
survivors b = LifeBoard (config b) $ V.zipWith (&&) cellsWLN (board b) 
              where cellsWLN = V.generate (V.length (board b)) (\i -> let p = (i `div` (fst (fst (config b))), i `mod` (fst (fst (config b)))) 
                                                                      in Prelude.elem (liveneighbs b p) [2,3])
                                                                      
births :: Board -> Board
births b = LifeBoard (config b) $ V.generate (V.length (board b)) (\i -> let p = (i `div` (fst (fst (config b))), i `mod` (fst (fst (config b)))) 
                                                                         in (isEmpty b p) && (liveneighbs b p == 3))
                                                                         
nextgen :: Board -> Board
nextgen b = LifeBoard (config b) $ V.zipWith (||)  (board (survivors b)) (board (births b))

