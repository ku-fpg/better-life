echo Building Life Engines...

make HuttonEngine > /dev/null
make HermitSetEngine &> /dev/null
make HermitQTreeEngine &> /dev/null
make HermitVectorEngine &> /dev/null
echo Done Building. Now running benchmarks...
echo Hutton Engine Benchmarks > results.txt
./examples/engines/HuttonEngine >> results.txt
echo -------------------------------------- >> results.txt
echo >> results.txt
echo HERMIT Converted Set Engine Benchmarks >> results.txt
./examples/HERMIT/HuttonEngineS >> results.txt
echo -------------------------------------- >> results.txt
echo >> results.txt
echo HERMIT Converted QuadTree Engine Benchmarks >> results.txt
./examples/HERMIT/HuttonEngineQ >> results.txt
echo -------------------------------------- >> results.txt
echo >> results.txt
echo HERMIT Converted Vector Engine Benchmarks >> results.txt
./examples/HERMIT/HuttonEngineV >> results.txt

echo Results saved to file results.txt. Now Cleaning...
make clean > /dev/null
echo Done!


