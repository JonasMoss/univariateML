set shell := ["bash", "-lc"]

repo_root := justfile_directory()

default: test

# Mirror cppunivariate/include/ into r/inst/include/. Required before
# R-side compilation because the R-package's PKG_CPPFLAGS = -I../inst/include
# expects the headers there. The r/inst/include/cppunivariate/ tree is
# treated as a generated mirror — do not edit it directly; gitignored.
sync-headers:
	mkdir -p r/inst/include
	rsync -a --delete cppunivariate/include/cppunivariate r/inst/include/

# Regenerate metadata-driven R sources (R/ml00_metadata.R and
# R/ml02_wrappers.R) from metadata/{models,docs}.json plus any
# per-distribution metadata.json files inside the cppunivariate
# distributions tree. Other R/ files are hand-written.
gen:
	cd r && \
	  UMVL_METADATA_DIR={{repo_root}}/metadata \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript scripts/codegen_metadata.R
	cd r && \
	  UMVL_METADATA_DIR={{repo_root}}/metadata \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript scripts/codegen_wrappers.R

regen: sync-headers gen
	cd r && Rscript -e "Rcpp::compileAttributes()"
	cd r && Rscript -e "devtools::document()"

test: regen
	cd r && UMVL_GOLDEN_DIR={{repo_root}}/quality/golden Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"

check: regen
	cd r && Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"

# Regenerate quality/golden/*.json from the current R/C++ pipeline.
# Run only when intentionally accepting a new baseline (e.g. algorithm
# change). After regen, inspect the diff before committing.
goldens: regen
	cd r && \
	  UMVL_METADATA_DIR={{repo_root}}/metadata \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_GOLDEN_DIR={{repo_root}}/quality/golden \
	  Rscript scripts/goldens.R

# A/B benchmark harness for Rcpp estimator variants.
bench model="burr" sizes="50,200,1000" reps="5":
	cd r && Rscript scripts/bench.R --model {{model}} --sizes {{sizes}} --reps {{reps}}

# Cross-check our dpqr against reference R packages (extraDistr, actuar,
# fGarch, logitnorm). Reference packages are NOT in DESCRIPTION;
# missing ones skip cleanly. Manual / development-time check.
dpqr-check:
	cd r && Rscript scripts/dpqr_check.R

# Build & run the standalone C++ test harness from cppunivariate/.
# This path does not require R.
cpp-test:
	cmake -S cppunivariate -B cppunivariate/build
	cmake --build cppunivariate/build
	cd cppunivariate/build && ctest --output-on-failure
