module LifeQC where

import Life.Types
import Life.Engine.Hutton
import Life.Engine.Set
import Life.Engine.UVector
import Life.Scenes

import Test.QuickCheck

-- Runs the Life (without display) for the specified number of generations
lifeHutton :: Int -> Config -> [Pos] -> Life.Engine.Hutton.Board
lifeHutton x c = (runLife x) . (scene c)

lifeSet :: Int -> Config -> [Pos] -> Life.Engine.Set.Board
lifeSet x c = (runLife x) . (scene c)

lifeUVector :: Int -> Config -> [Pos] -> Life.Engine.UVector.Board
lifeUVector x c = (runLife x) . (scene c)


-- QuickCheck Properties
testHS x c p = alive (lifeHutton x c p) == alive (lifeSet x c p)

testHV x c p = alive (lifeHutton x c p) == alive (lifeUVector x c p)
