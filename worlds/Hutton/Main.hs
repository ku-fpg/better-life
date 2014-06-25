module Main where

import Life.Types 
import Life.Console
import Life.Worlds

import qualified Life

data Board = Board Life.Env Life.Board

instance Life Board where
  empty (w,h) = Board (w,h,torus_surface (w,h)) []
  diff = undefined
  next  (Board env brd) = Board env (Life.nextgen env brd)
  inv = undefined
  size  (Board (w,h,_) _)  = (w,h)
  alive (Board _ brd) = brd


torus_surface :: (Int,Int) -> Pos -> Pos
torus_surface (w,h) (x,y) = (((x-1) `mod` w) + 1, ((y-1) `mod` h + 1))

main :: IO ()
main = lifeConsole (glider (16,16) :: Board)
