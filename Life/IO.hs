module Life.IO where

import Life.Types

import Control.Applicative

-- These functions need to be rewritten to work with new Life board class

{-
readLife :: Life board => FilePath -> IO board
readLife fname = do 
	board <- (dropWhile ((/= "#P") . head) . map words . lines) <$> readFile fname
	let [_, x, y] = head board
	let board' = concat $ tail board
	let xdim = (maximum . map length) board'
	let ydim = length board'
	let pairs = [(xcoord, ycoord) | ycoord <- [1..ydim], xcoord <- [1..xdim]] 
	return $ foldl (\acc arg -> if ((/= '.') . snd) arg
					then (fst arg) : acc
					else acc)
			[] 
			$ zip pairs $ concat board'

writeLife :: Life board => board -> FilePath -> IO ()
writeLife board fname = do 
	let mincorner = minimum board
	let maxcorner = maximum board
	let xdim = (fst maxcorner) - (fst mincorner) + 1
	let window = [(xcoord, ycoord) | ycoord <- [1..(snd maxcorner)], xcoord <- [1..xdim]]
	writeFile fname  ("#Life 1.05 \n#P 1 1\n" ++ (concatMap (\arg -> if arg `elem` board 
									then if (fst arg) `mod` xdim == 0 
										then "*\n" 
										else "*"
									else if (fst arg) `mod` xdim == 0 
										then ".\n"
										else ".")
								window))
-}
