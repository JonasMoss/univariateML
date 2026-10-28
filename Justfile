set shell := ["bash", "-lc"]

repo_root := justfile_directory()

default: test

# Wipe and rebuild univariateML/package/ from workspace sources.
# package/ is the CRAN-shippable artefact, fully generated and
# gitignored; univariateML/ itself holds only sources, scripts, and
# metadata.
assemble:
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript scripts/assemble_package.R

# Mirror cppunivariate/include/ into univariateML/package/inst/include/.
# Runs after assemble (which wipes package/), required before R-side
# compilation because the package's PKG_CPPFLAGS = -I../inst/include
# expects the headers there.
sync-headers: assemble
	mkdir -p univariateML/package/inst/include
	rsync -a --delete --prune-empty-dirs --include='*/' --include='*.hpp' --exclude='*' \
	  cppunivariate/include/cppunivariate univariateML/package/inst/include/

# Generate metadata-driven R sources into package/R/, then run
# Rcpp::compileAttributes() in package/ to produce RcppExports.{R,cpp}.
# Inputs:
#   - cppunivariate/.../<id>/metadata.hpp + univariateML/metadata/<id>.json
#                                    (canonical per-distribution form)
#   - cppunivariate/.../<id>/metadata.json
#                                    (intermediate per-distribution form)
gen: sync-headers
	cd univariateML/package && Rscript -e "Rcpp::compileAttributes()"
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

regen: gen
	cd univariateML/package && Rscript -e "roxygen2::roxygenise()"

test: regen
	cd univariateML/package && \
	  UMVL_GOLDEN_DIR={{repo_root}}/quality/golden \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript -e "testthat::test_local('.', stop_on_failure = TRUE)"

check: regen
	cd univariateML/package && R CMD build --no-build-vignettes . && R CMD check --no-manual --ignore-vignettes univariateML_*.tar.gz

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

# --- LaTeX research notes (notes/) ---
notes_dir := "notes"

# Build one note (named, or the most recently edited) and open the PDF.
note-build name="":
    #!/usr/bin/env bash
    set -euo pipefail
    dir="{{notes_dir}}"
    if [ -n "{{name}}" ]; then
        tex="$dir/$(basename "{{name}}" .tex).tex"
    else
        tex="$(ls -t "$dir"/*.tex 2>/dev/null | head -1 || true)"
    fi
    [ -n "${tex:-}" ] && [ -f "$tex" ] || { echo "no .tex note found in $dir"; exit 1; }
    base="$(basename "$tex" .tex)"
    cd "$dir"
    pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null
    pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null
    rm -f "$base".{aux,log,out,toc}
    if command -v xdg-open >/dev/null 2>&1; then opener=xdg-open; else opener=open; fi
    if command -v setsid >/dev/null 2>&1; then
        setsid "$opener" "$base.pdf" >/dev/null 2>&1 < /dev/null &
    else
        "$opener" "$base.pdf" >/dev/null 2>&1 &
    fi
    echo "built and opened $dir/$base.pdf"

# Rebuild every note (no viewer); report any that fail to compile.
note-build-all:
    #!/usr/bin/env bash
    set -uo pipefail
    dir="{{notes_dir}}"
    fail=0
    shopt -s nullglob
    for tex in "$dir"/*.tex; do
        base="$(basename "$tex" .tex)"
        if ( cd "$dir" && pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null 2>&1 \
                       && pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null 2>&1 ); then
            ( cd "$dir" && rm -f "$base".{aux,log,out,toc} )
            echo "ok   $base"
        else
            echo "FAIL $base"; fail=1
        fi
    done
    exit $fail
