{- Game of life example from section 9.7 of Programming in Haskell,
   Graham Hutton, Cambridge University Press, 2007.

   Edited by Andy Gill.
    - using thread delay
    - using standard combinators like sequence and nub

  Edited by Brad Torrence
  - added Env, the environment is now an argument
-}

module Life where

import Data.List

-- Game of life
type Pos = (Int,Int)
type Board = [Pos]
type Warp = Pos -> Pos
type Env = (Int,Int,Warp)

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

neighbs :: Env -> Pos -> [Pos]
neighbs (width,height,warp) (x,y) = 
	sort $ filter (\(x,y) -> (x > 0 && x <= width) && (y > 0 && y <= height))
	     $ map warp [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

liveneighbs :: Env -> Board -> Pos -> Int
liveneighbs e b = length . filter (isAlive b) . (neighbs e)

survivors :: Env -> Board -> [Pos]
survivors e b = [ p | p <- b, elem (liveneighbs e b p) [2,3] ]

births :: Env -> Board -> [Pos]
births e b = [ p | p <- nub (concat (map (neighbs e) b)), isEmpty b p, liveneighbs e b p == 3 ]

nextgen :: Env -> Board -> Board
nextgen e b = survivors e b ++ births e b

