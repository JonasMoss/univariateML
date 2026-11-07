set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-scalar-trio-data/check.cpp -o build/scratch/scalar-check
build/scratch/scalar-check > dev/research/evaluations/2026-11-07-scalar-trio-data/check-output.txt
python3 dev/research/evaluations/2026-11-07-scalar-trio-data/check.py dev/research/evaluations/2026-11-07-scalar-trio-data/check-output.txt > dev/research/evaluations/2026-11-07-scalar-trio-data/independent.json
python3 dev/research/evaluations/2026-11-07-scalar-trio-data/bounds.py dev/research/evaluations/2026-11-07-scalar-trio-data/check-output.txt > dev/research/evaluations/2026-11-07-scalar-trio-data/bounds.json
