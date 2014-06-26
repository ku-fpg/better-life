module Life.Worlds where

import Life.Types
import Data.List

-- Pre-plotted Scenes

glider :: [Pos]
glider = [(2,3),(3,4),(4,2),(4,3),(4,4)]

gliderGun :: [Pos]
gliderGun = sort [(2,6), (3,6), (2,7), (3,7), (14,4), 
				(15,4), (17,5), (18,6), (18,7), (18,8), 
				(19,7), (17,9), (16,7), (15,10), (14,10), 
				(13,9), (12,8), (12,7), (12,6), (13,5), 
				(22,4), (22,5), (22,6), (23, 4), (23,5), 
				(23,6), (24,3), (24,7), (26,2), (26,3), 
				(26,7), (26,8), (36,4), (36,5), (37,4), (37,5)]

-- Warping Functions

torusSurface :: Size -> Pos -> Pos
torusSurface (w,h) (x,y) = ((x-1) `mod` w + 1, (y-1) `mod` h + 1)

flatSurface :: Pos -> Pos
flatSurface = id

