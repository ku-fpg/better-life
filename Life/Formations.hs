module Life.Formations where

import Life.Types

glider :: Pos -> Scene
glider (x,y) = [(x+2,y+3),(x+3,y+4),(x+4,y+2),(x+4,y+3),(x+4,y+4)]

gliders :: Pos -> Scene
gliders (x,y) = scenes [glider (x,y), glider (x+5,y), glider (x,y+5), glider (x+5,y+5)]

gliders2 :: Pos -> Scene
gliders2 (x,y) = scenes [gliders (x,y), gliders (x+10,y), gliders (x,y+10), gliders (x+10,y+10)]

gliders3 :: Pos -> Scene
gliders3 (x,y) = scenes [gliders2 (x,y), gliders2 (x+20,y), gliders2 (x,y+20), gliders2 (x+20,y+20)]

gliders4 :: Pos -> Scene
gliders4 (x,y) = scenes [gliders3 (x,y), gliders3 (x+40,y), gliders3 (x,y+40), gliders3 (x+40,y+40)]

gliders5 :: Pos -> Scene
gliders5 (x,y) = scenes [gliders4 (x,y), gliders4 (x+80,y), gliders4 (x,y+80), gliders4 (x+80,y+80)]

gliderGunL :: Pos -> Scene
gliderGunL (x,y) = [ (x+2,y+6), (x+2,y+7), (x+3,y+6), (x+3,y+7), 
					(x+12,y+6), (x+12,y+7), (x+12,y+8), (x+13,y+5), 
					(x+13,y+9), (x+14,y+4), (x+14,y+10), (x+15,y+4), 
					(x+15,y+10), (x+16,y+7), (x+17,y+5), (x+17,y+9), 
					(x+18,y+6), (x+18,y+7), (x+18,y+8), (x+19,y+7),
					(x+22,y+4), (x+22,y+5), (x+22,y+6), (x+23,y+4), 
					(x+23,y+5), (x+23,y+6), (x+24,y+3), (x+24,y+7), 
					(x+26,y+2), (x+26,y+3), (x+26,y+7), (x+26,y+8), 
					(x+36,y+4), (x+36,y+5), (x+37,y+4), (x+37,y+5)]

gliderGunR :: Pos -> Scene
gliderGunR (x,y) = [ (x+37,y+6), (x+37,y+7), (x+36,y+6), (x+36,y+7), 
					(x+27,y+6), (x+27,y+7), (x+27,y+8), (x+26,y+5), 
					(x+26,y+9), (x+25,y+4), (x+25,y+10), (x+24,y+4), 
					(x+24,y+10), (x+23,y+7), (x+22,y+5), (x+22,y+9), 
					(x+21,y+6), (x+21,y+7), (x+21,y+8), (x+20,y+7),
					(x+17,y+4), (x+17,y+5), (x+17,y+6), (x+16,y+4), 
					(x+16,y+5), (x+16,y+6), (x+15,y+3), (x+15,y+7), 
					(x+13,y+2), (x+13,y+3), (x+13,y+7), (x+13,y+8), 
					(x+3,y+4), (x+3,y+5), (x+2,y+4), (x+2,y+5)]

gguns :: Pos -> Scene
gguns (x,y) = scenes [gliderGunL (x,y), gliderGunR (x+50,y+15)]

battle :: Pos -> Scene
battle (x,y) = scenes [gliderGunL (x,y), gliderGunR (x+50,y)]

acorn :: Pos -> Scene
acorn (x,y) = [(x+20,y+15),(x+21,y+13),(x+22,y+10),(x+22,y+11),(x+22,y+12),(x+22,y+15),(x+22,y+16)]


