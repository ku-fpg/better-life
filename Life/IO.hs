module Life.IO where

import Control.Applicative

import Life.Types

-- These functions have not been tested

readLife :: Life board => FilePath -> IO board
readLife fname = do 
	b <- (dropWhile ((/= "#P") . head) . map words . lines) <$> readFile fname
	let [width, height, w] = head b
	let xdim = read width
	let ydim = read height
	return $ scene ((xdim, ydim), if w == "0" then False else True ) $ 
		foldl (\acc arg -> if ((== '*') . snd) arg
					then (fst arg) : acc
					else acc) 
			[] 
			$ zip [ (x,y) | y <- [0..ydim-1], x <- [0..xdim-1] ] $ concat $ concat $ tail b

writeLife :: Life board => board -> FilePath -> IO ()
writeLife b fname = do 
	let xdim = fst $ fst $ config b
	let ydim = snd $ fst $ config b
	let warp = if snd $ config b then "1" else "0"
	writeFile fname $ "#Life 1.05 \n#P " ++ 
		show xdim ++ " " ++ show ydim ++ " " ++ warp ++ "\n" ++ 
		concatMap (\arg -> if (fst arg) < xdim - 1
					then if arg `elem` (alive b)
						then "*"
						else "."
					else if arg `elem` (alive b)
						then "*\n"
						else ".\n")
			[ (x,y) | y <- [0..ydim-1], x <- [0..xdim-1] ]


