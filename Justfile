set shell := ["bash", "-lc"]

default: test

# Regenerate the metadata-driven R sources (R/ml00_metadata.R and
# R/ml02_wrappers.R) from metadata/{models,docs}.json. Other R/ files
# are hand-written and live there directly — no codegen pass.
gen:
	Rscript scripts/codegen_metadata.R && \
	Rscript scripts/codegen_wrappers.R

regen: gen
	Rscript -e "Rcpp::compileAttributes()"
	Rscript -e "devtools::document()"

test: regen
	Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"

check: regen
	Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"

# Regenerate tests/golden/*.json from the current R/C++ pipeline. Run
# only when intentionally accepting a new baseline (e.g. algorithm
# change). After regen, inspect the diff before committing.
goldens:
	Rscript scripts/goldens.R

# A/B benchmark harness for Rcpp estimator variants.
bench model="burr" sizes="50,200,1000" reps="5":
	Rscript scripts/bench.R --model {{model}} --sizes {{sizes}} --reps {{reps}}

# Build & run the standalone C++ test harness (mlcore_tests + bfgs_tests + sample_tests).
cpp-test:
	cmake -S tests/cpp -B tests/cpp/build
	cmake --build tests/cpp/build
	cd tests/cpp/build && ctest --output-on-failure
