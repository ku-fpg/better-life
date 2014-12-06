HuttonEngine: 
	ghc examples/engines/HuttonEngine.hs 

HuttonConsole:
	ghc examples/simulations/HuttonConsole.hs

HuttonCanvas:
	ghc -threaded examples/simulations/HuttonCanvas.hs

SetEngine:
	ghc examples/engines/SetEngine.hs 

SetConsole:
	ghc examples/simulations/SetConsole.hs

SetCanvas:
	ghc -threaded examples/simulations/SetCanvas.hs

VectorEngine:
	ghc examples/engines/UVectorEngine.hs

VectorCanvas:
	ghc -threaded examples/simulations/UVectorCanvas.hs

QTreeEngine:
	ghc examples/engines/QTreeEngine.hs

QTreeCanvas:
	ghc -threaded examples/simulations/QTreeCanvas.hs

HermitSetEngine:
	hermit examples/HERMIT/HuttonEngineS.hs +Life.Engine.HERMIT.HuttonS HERMIT/Set/lifeclass.hss 

HermitSetCanvas:
	hermit examples/HERMIT/HuttonCanvasS.hs +Life.Engine.HERMIT.HuttonS HERMIT/Set/lifeclass.hss -- -threaded

HermitQTreeEngine:
	hermit examples/HERMIT/HuttonEngineQ.hs +Life.Engine.HERMIT.HuttonQ HERMIT/QTree/lifeclass.hss 

HermitVectorEngine:
	hermit examples/HERMIT/HuttonEngineV.hs +Life.Engine.HERMIT.HuttonV HERMIT/UVector/lifeclass.hss

HermitAccEngine:
	hermit examples/HERMIT/HuttonEngineA.hs +Life.Engine.HERMIT.HuttonA HERMIT/Acc/lifeclass.hss -- -threaded

HermitAccCanvas:
	hermit examples/HERMIT/HuttonCanvasA.hs +Life.Engine.HERMIT.HuttonA HERMIT/Acc/lifeclass.hss -- -threaded

hermitbench:
	bash hermit_tests.sh

clean:
	rm -rf examples/engines/HuttonEngine examples/simulations/HuttonCanvas examples/simulations/HuttonConsole
	rm -rf examples/engines/SetEngine examples/simulations/SetCanvas examples/simulations/SetConsole
	rm -rf examples/engines/UVectorEngine examples/simulations/UVectorCanvas
	rm -rf examples/engines/QTreeEngine examples/simulations/QTreeCanvas
	rm -rf examples/HERMIT/HuttonEngineA examples/HERMIT/HuttonCanvasA
	rm -rf examples/HERMIT/HuttonEngineS examples/HERMIT/HuttonCanvasS
	rm -rf examples/HERMIT/HuttonEngineV
	rm -rf examples/HERMIT/HuttonEngineQ
	rm -rf examples/HERMIT/*~ examples/HERMIT/*.hi examples/HERMIT/*.o
	rm -rf examples/engines/*~ examples/engines/*.hi examples/engines/*.o
	rm -rf examples/simulations/*~ examples/simulations/*.hi examples/simulations/*.o
	rm -rf examples/*~ examples/*.hi examples/*.o
	rm -rf *~ Reference/*~ Life/*~ Life/*.hi Life/*.o
	rm -rf Life/Engine/*~ Life/Engine/*.hi Life/Engine/*.o
	rm -rf Life/Engine/HERMIT/*~ Life/Engine/HERMIT/*.hi Life/Engine/HERMIT/*.o
	rm -rf Life/Display/*~ Life/Display/*.hi Life/Display/*.o
	rm -rf HERMIT/Set/scripts/*~
	rm -rf HERMIT/Set/*~ HERMIT/Set/*.hi HERMIT/Set/*.o
	rm -rf HERMIT/UVector/scripts/*~
	rm -rf HERMIT/UVector/*~ HERMIT/UVector/*.hi HERMIT/UVector/*.o
	rm -rf HERMIT/QTree/scripts/*~
	rm -rf HERMIT/QTree/*~ HERMIT/QTree/*.hi HERMIT/QTree/*.o
	rm -rf HERMIT/Acc/scripts/*~
	rm -rf HERMIT/Acc/*~ HERMIT/Acc/*.hi HERMIT/Acc/*.o

