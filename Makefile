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

HermitSetEngine:
	cd sandbox; hermit examples/HuttonEngineS.hs +Life.Engine.HuttonS HERMIT/Set/lifeclass.hss

HermitSetCanvas:
	cd sandbox ; hermit examples/HuttonCanvasS.hs +Life.Engine.HuttonS HERMIT/Set/lifeclass.hss -- -threaded

HermitVectorEngine:
	cd sandbox; hermit examples/HuttonEngineV.hs +Life.Engine.HuttonV HERMIT/UVector/lifeclass.hss

HermitVectorCanvas:
	cd sandbox ; hermit examples/HuttonCanvasV.hs +Life.Engine.HuttonV HERMIT/UVector/lifeclass.hss -- -threaded

clean:
	rm -rf examples/HuttonEngine examples/HuttonCanvas
	rm -rf examples/SetEngine examples/SetCanvas
	rm -rf examples/UVectorEngine examples/UVectorCanvas
	rm -rf examples/QTreeEngine examples/QTreeCanvas
	rm -rf sandbox/examples/HuttonEngineS sandbox/examples/HuttonCanvasS
	rm -rf sandbox/examples/HuttonEngineV sandbox/examples/HuttonCanvasV
	rm -rf sandbox/examples/HuttonEngineQ sandbox/examples/HuttonCanvasQ
	rm -rf examples/*~ examples/*.hi examples/*.o
	rm -rf *~ Reference/*~ Life/*~ Life/*.hi Life/*.o
	rm -rf Life/Engine/*~ Life/Engine/*.hi Life/Engine/*.o
	rm -rf Life/Display/*~ Life/Display/*.hi Life/Display/*.o
	rm -rf sandbox/examples/*~ sandbox/examples/*.hi sandbox/examples/*.o
	rm -rf sandbox/Life/*~ sandbox/Life/*.hi sandbox/Life/*.o
	rm -rf sandbox/Life/Engine/*~ sandbox/Life/Engine/*.hi sandbox/Life/Engine/*.o
	rm -rf sandbox/Life/Display/*~ sandbox/Life/Display/*.hi sandbox/Life/Display/*.o
	rm -rf sandbox/HERMIT/Set/scripts/*~
	rm -rf sandbox/HERMIT/Set/*~ sandbox/HERMIT/Set/*.hi sandbox/HERMIT/Set/*.o
	rm -rf sandbox/HERMIT/UVector/scripts/*~
	rm -rf sandbox/HERMIT/UVector/*~ sandbox/HERMIT/UVector/*.hi sandbox/HERMIT/UVector/*.o
	rm -rf sandbox/HERMIT/QTree/scripts/*~
	rm -rf sandbox/HERMIT/QTree/*~ sandbox/HERMIT/QTree/*.hi sandbox/HERMIT/QTree/*.o

