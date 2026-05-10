set shell := ["bash", "-lc"]

repo_root := justfile_directory()

default: test

# Wipe and rebuild univariateML/package/ from workspace sources.
# package/ is the CRAN-shippable artefact, fully generated and
# gitignored; univariateML/ itself holds only sources, scripts, and
# metadata.
assemble:
	cd univariateML && Rscript scripts/assemble_package.R

# Mirror cppunivariate/include/ into univariateML/package/inst/include/.
# Runs after assemble (which wipes package/), required before R-side
# compilation because the package's PKG_CPPFLAGS = -I../inst/include
# expects the headers there.
sync-headers: assemble
	mkdir -p univariateML/package/inst/include
	rsync -a --delete cppunivariate/include/cppunivariate univariateML/package/inst/include/

# Generate metadata-driven R sources into package/R/, then run
# Rcpp::compileAttributes() in package/ to produce RcppExports.{R,cpp}.
# Inputs:
#   - cppunivariate/.../<id>/metadata.hpp + univariateML/metadata/<id>.json
#                                    (canonical per-distribution form)
#   - cppunivariate/.../<id>/metadata.json
#                                    (intermediate per-distribution form)
gen: sync-headers
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_R_METADATA_DIR={{repo_root}}/univariateML/metadata \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML/package/R \
	  Rscript scripts/codegen_metadata.R
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_R_METADATA_DIR={{repo_root}}/univariateML/metadata \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML/package/R \
	  Rscript scripts/codegen_wrappers.R
	cd univariateML/package && Rscript -e "Rcpp::compileAttributes()"

regen: gen
	cd univariateML/package && Rscript -e "devtools::document()"

test: regen
	cd univariateML/package && \
	  UMVL_GOLDEN_DIR={{repo_root}}/quality/golden \
	  Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"

check: regen
	cd univariateML/package && Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"

# Regenerate quality/golden/*.json from the current R/C++ pipeline.
# Run only when intentionally accepting a new baseline (e.g. algorithm
# change). After regen, inspect the diff before committing.
goldens: regen
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_R_METADATA_DIR={{repo_root}}/univariateML/metadata \
	  UMVL_GOLDEN_DIR={{repo_root}}/quality/golden \
	  UMVL_PKG_DIR={{repo_root}}/univariateML/package \
	  Rscript scripts/goldens.R

# Cross-check our dpqr against reference R packages (extraDistr, actuar,
# fGarch, logitnorm). Reference packages may not all be in DESCRIPTION;
# missing ones skip cleanly. Manual / development-time check.
dpqr-check: regen
	cd univariateML && \
	  UMVL_PKG_DIR={{repo_root}}/univariateML/package \
	  Rscript scripts/dpqr_check.R

# Build & run the standalone C++ test harness from cppunivariate/.
# This path does not require R.
cpp-test:
	cmake -S cppunivariate -B cppunivariate/build
	cmake --build cppunivariate/build
	cd cppunivariate/build && ctest --output-on-failure

# AI-first validation harness. Symbolic spec lives at quality/math/<id>.py;
# `derive` emits eval_request.json + symbolic_grid.json into
# quality/derived/<id>/. The validation_tests ctest target reads
# eval_request.json and writes cpp_eval.json on every `cpp-test` run;
# `validate` then diffs the symbolic and C++ grids and emits per-check
# reports + quality_report.md. Run `derive` after writing or changing a
# spec; run `validate` after any C++ change to a Cost struct.
derive id:
	PYTHONPATH={{repo_root}} python3 -m quality.harness derive {{id}}

validate id: cpp-test
	PYTHONPATH={{repo_root}} python3 -m quality.harness validate {{id}}
