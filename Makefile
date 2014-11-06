HuttonEngine: 
	ghc examples/HuttonEngine.hs

HuttonCanvas:
	ghc -threaded examples/HuttonCanvas.hs

SetEngine:
	ghc examples/SetEngine.hs

SetCanvas:
	ghc -threaded examples/SetCanvas.hs

VectorEngine:
	ghc examples/UVectorEngine.hs

VectorCanvas:
	ghc -threaded examples/UVectorCanvas.hs

QTreeEngine:
	ghc examples/QTreeEngine.hs

QTreeCanvas:
	ghc -threaded examples/QTreeCanvas.hs

clean:
	rm -rf examples/HuttonEngine examples/HuttonCanvas
	rm -rf examples/SetEngine examples/SetCanvas
	rm -rf examples/UVectorEngine examples/UVectorCanvas
	rm -rf examples/QTreeEngine examples/QTreeCanvas
	rm -rf examples/*~ examples/*.hi examples/*.o
	rm -rf *~ Reference/*~ Life/*~ Life/*.hi Life/*.o
	rm -rf Life/Engine/*~ Life/Engine/*.hi Life/Engine/*.o
	rm -rf Life/Display/*~ Life/Display/*.hi Life/Display/*.o

