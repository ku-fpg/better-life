module Life (nextgen, Pos, Board) where
{- Game of life example from section 9.7 of Programming in Haskell,
   Graham Hutton, Cambridge University Press, 2007.

   Edited by Andy Gill for EECS 876.
    - using thread delay
    - using standard combinators like sequence and nub
-}


    import Data.List
    
    -- Game of life
    type Pos                      = (Int,Int)
    
    width                         :: Int
    width                         =  20
    
    height                        :: Int
    height                        =  20
    
    type Board                    =  [Pos]
    
    isAlive                       :: Board -> Pos -> Bool
    isAlive b p                   =  elem p b
    
    isEmpty                       :: Board -> Pos -> Bool
    isEmpty b p                   =  not (isAlive b p)
    
    neighbs                       :: Pos -> [Pos]
    neighbs (x,y)                 =  map wrap [(x-1,y-1), (x,y-1),
                                               (x+1,y-1), (x-1,y),
                                               (x+1,y)  , (x-1,y+1),
                                               (x,y+1)  , (x+1,y+1)] 
    
    wrap                          :: Pos -> Pos
    wrap (x,y)                    =  (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))
    
    liveneighbs                   :: Board -> Pos -> Int
    liveneighbs b                 =  length . filter (isAlive b) . neighbs
    
    survivors                     :: Board -> [Pos]
    survivors b                   =  [p | p <- b, elem (liveneighbs b p) [2,3]]
    
    births b                      =  [p | p <- nub (concat (map neighbs b)),
                                          isEmpty b p,
                                          liveneighbs b p == 3]
    
    nextgen                       :: Board -> Board
    nextgen b                     =  survivors b ++ births b
    
    
    
    