make HuttonEngine;
make HermitSetEngine;
make HermitQTreeEngine;
make HermitVectorEngine;
echo Done Building... Now running benchmarks.;

echo Hutton Engine Benchmarks > results.txt;
./examples/engines/HuttonEngine >> results.txt;
echo -------------------------------------- >> results.txt;
echo >> results.txt;
echo HERMIT Converted Set Engine Benchmarks >> results.txt;
./examples/HERMIT/HuttonEngineS >> results.txt;
echo -------------------------------------- >> results.txt;
echo >> results.txt;
echo HERMIT Converted QuadTree Engine Benchmarks >> results.txt;
./examples/HERMIT/HuttonEngineQ >> results.txt;
echo -------------------------------------- >> results.txt;
echo >> results.txt;
echo HERMIT Converted Vector Engine Benchmarks >> results.txt;
./examples/HERMIT/HuttonEngineV >> results.txt;

make clean;

