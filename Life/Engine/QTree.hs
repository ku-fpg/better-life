{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Life.Engine.QTree where

import Data.QuadTree
import Data.Boolean (xor)

import Life.Types

type Board = LifeBoard Config (QuadTree Bool)

neighbs :: Config -> Pos -> [Pos]
neighbs ((w,h),warp) (x,y) = if warp
		then map (\(x,y) -> (x `mod` w, y `mod` h)) neighbors
		else filter (\(x,y) -> (x >= 0 && x < w) && (y >= 0 && y < h)) neighbors
	where neighbors = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

isAlive :: Board -> Pos -> Bool
isAlive b p = getLocation p $ board b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . (isAlive b)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (config b))

indices :: Size -> [Pos]
indices (w,h) = [ (x,y) | x<-[0..w-1], y<-[0..h-1] ]

survivors :: Board -> Board
survivors b = LifeBoard cb $ foldr (\p qt -> setLocation p qt (isAlive b p && elem (liveneighbs b p) [2,3])) (makeTree sz False) $ indices sz
	where
		cb = config b
		sz = fst cb

births :: Board -> Board
births b = LifeBoard cb $ foldr (\p qt -> setLocation p qt (isEmpty b p && liveneighbs b p == 3)) 
			(makeTree sz False) 
			$ indices sz
	where 
		cb = config b
		sz = fst cb

nextgen :: Board -> Board
nextgen b = LifeBoard cb $ foldr (\p qt -> setLocation p qt (getLocation p (board (survivors b)) || getLocation p (board (births b)))) (makeTree sz False) $ indices sz
	where 
		cb = config b
		sz = fst cb

instance Life Board where
	empty c = LifeBoard c $ makeTree (fst c) False
	alive b = [ p | p <- indices (treeDimensions bb), getLocation p bb ]
		where bb = board b
	inv p b = LifeBoard (config b) $ setLocation p bb $ not $ getLocation p bb
		where bb = board b
	next b = nextgen b


