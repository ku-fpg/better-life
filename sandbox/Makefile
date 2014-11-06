HuttonEngine: 
	ghc examples/HuttonEngine.hs

HuttonCanvas:
	ghc -threaded examples/HuttonCanvas.hs

SetEngine:
	ghc examples/SetEngine.hs

VectorEngine:
	ghc examples/UVectorEngine.hs

QTreeEngine:
	ghc examples/QTreeEngine.hs

HermitSetEngine:
	hermit examples/HuttonEngineS.hs +Life.Engine.HuttonS HERMIT/Set/lifeclass.hss

HermitSetCanvas:
	hermit examples/HuttonCanvasS.hs +Life.Engine.HuttonS HERMIT/Set/lifeclass.hss -- -threaded

HermitVectorEngine:
	hermit examples/HuttonEngineV.hs +Life.Engine.HuttonV HERMIT/UVector/lifeclass.hss

HermitVectorCanvas:
	hermit examples/HuttonCanvasV.hs +Life.Engine.HuttonV HERMIT/UVector/lifeclass.hss -- -threaded

HermitQTreeEngine:
	hermit examples/HuttonEngineQ.hs +Life.Engine.HuttonV HERMIT/QTree/lifeclass.hss

HermitAccEngine:
	hermit examples/HuttonEngineA.hs +Life.Engine.HuttonA HERMIT/Acc/lifeclass.hss -- -threaded

HermitAccCanvas:
	hermit examples/HuttonCanvasA.hs +Life.Engine.HuttonA HERMIT/Acc/lifeclass.hss -- -threaded

clean:
	rm -rf examples/HuttonEngine examples/HuttonCanvas
	rm -rf examples/SetEngine examples/SetCanvas
	rm -rf examples/UVectorEngine examples/UVectorCanvas
	rm -rf examples/QTreeEngine examples/QTreeCanvas
	rm -rf examples/HuttonEngineA examples/HuttonCanvasA
	rm -rf examples/HuttonEngineS examples/HuttonCanvasS
	rm -rf examples/HuttonEngineV examples/HuttonCanvasV
	rm -rf examples/HuttonEngineQ examples/HuttonCanvasQ
	rm -rf examples/*~ examples/*.hi examples/*.o
	rm -rf *~ Reference/*~ Life/*~ Life/*.hi Life/*.o
	rm -rf Life/Engine/*~ Life/Engine/*.hi Life/Engine/*.o
	rm -rf Life/Display/*~ Life/Display/*.hi Life/Display/*.o
	rm -rf examples/*~ examples/*.hi examples/*.o
	rm -rf Life/*~ Life/*.hi Life/*.o
	rm -rf Life/Engine/*~ Life/Engine/*.hi Life/Engine/*.o
	rm -rf Life/Display/*~ Life/Display/*.hi Life/Display/*.o
	rm -rf HERMIT/Set/scripts/*~
	rm -rf HERMIT/Set/*~ HERMIT/Set/*.hi HERMIT/Set/*.o
	rm -rf HERMIT/UVector/scripts/*~
	rm -rf HERMIT/UVector/*~ HERMIT/UVector/*.hi HERMIT/UVector/*.o
	rm -rf HERMIT/QTree/scripts/*~
	rm -rf HERMIT/QTree/*~ HERMIT/QTree/*.hi HERMIT/QTree/*.o
	rm -rf HERMIT/Acc/scripts/*~
	rm -rf HERMIT/Acc/*~ HERMIT/Acc/*.hi HERMIT/Acc/*.o

