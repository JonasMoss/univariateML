set shell := ["bash", "-lc"]

default: test

clean-r:
	find R -maxdepth 1 -type f ! -name 'RcppExports.R' -delete

gen:
	Rscript scripts/gen_metadata.R && \
	Rscript scripts/gen_estimators_from_rsrc.R && \
	Rscript scripts/gen_wrappers.R && \
	Rscript scripts/gen_utils_copy.R

regen: clean-r gen
	Rscript -e "Rcpp::compileAttributes()"
	Rscript -e "devtools::document()"

test: regen
	Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"

check: regen
	Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"

bench model="burr" sizes="50,200,1000" reps="5":
	Rscript scripts/bench_cpp.R --model {{model}} --sizes {{sizes}} --reps {{reps}}

# Regenerate tests/golden/*.json from the current R/C++ pipeline. Run this
# only when you intentionally accept a new baseline (e.g. an algorithm
# change). After regen, inspect the diff before committing.
goldens:
	Rscript scripts/gen_goldens.R

# Build & run the standalone C++ test harness (mlcore_tests).
# Currently links against the system R install; WS3 task #4 will swap in
# a vendored standalone Rmath so this works without R on PATH.
cpp-test:
	cmake -S tests/cpp -B tests/cpp/build
	cmake --build tests/cpp/build
	cd tests/cpp/build && ctest --output-on-failure
