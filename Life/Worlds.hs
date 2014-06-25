module Life.Worlds where

import Life.Types

glider :: Life board => (Int,Int) -> board
glider sz = scene sz [(2,3),(3,4),(4,2),(4,3),(4,4)]
