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

# Run Rcpp::compileAttributes() in package/ (RcppExports.{R,cpp} from the
# per-distribution adapters), then generate package/R/ml00_metadata.R and
# ml02_wrappers.R (with roxygen docs) from distributions/<id>/README.md.
gen: sync-headers
	cd univariateML/package && Rscript -e "Rcpp::compileAttributes()"
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML/package/R \
	  Rscript scripts/codegen_metadata.R
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML/package/R \
	  Rscript scripts/codegen_wrappers.R

regen: gen
	cd univariateML/package && Rscript -e "roxygen2::roxygenise()"

test: regen
	cd univariateML/package && \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript -e "testthat::test_local('.', stop_on_failure = TRUE)"

check: regen
	cd univariateML/package && R CMD build --no-build-vignettes . && R CMD check --no-manual --ignore-vignettes univariateML_*.tar.gz

# Regenerate distributions/<id>/golden.json from the current R/C++ pipeline.
# Run only when intentionally accepting a new baseline (e.g. algorithm
# change). After regen, inspect the diff before committing.
goldens: regen
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_PKG_DIR={{repo_root}}/univariateML/package \
	  Rscript scripts/goldens.R

# Rewrite distributions/<id>/reference.json: frozen values from the former
# reference packages (fGarch, logitnorm, nakagami, sads) of the families
# with their own d/p/q/r. Needs those packages; the tests do not.
reference-values: regen
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  UMVL_PKG_DIR={{repo_root}}/univariateML/package \
	  Rscript scripts/reference_values.R

# Rewrite the distribution tables in univariateML/README.Rmd and README.md
# from distributions/<id>/README.md.
readme-table:
	cd univariateML && \
	  UMVL_DOSSIER_DIR={{repo_root}}/cppunivariate/include/cppunivariate/distributions \
	  Rscript scripts/readme_table.R

# Build & run the standalone C++ test harness from cppunivariate/.
# This path does not require R.
cpp-test:
	cmake -S cppunivariate -B cppunivariate/build
	cmake --build cppunivariate/build
	cd cppunivariate/build && ctest --output-on-failure

# Check real fitter goldens against symbolic README densities.
quality *ids:
	PYTHONPATH={{repo_root}} python3 -m quality check {{ids}}

# Independent high-precision d/p/q truth from declarative densities.
dpqr-truth *ids:
	PYTHONPATH={{repo_root}} python3 -m quality truth {{ids}}

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
